{-# LANGUAGE OverloadedStrings #-}
module Ingest.Types
  where

import           Control.Monad (guard)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Key as JK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isControl, isDigit)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word64)

import Type
import Field
import Catalog

data IngestJoin
  = IngestHaloJoin -- ^Indicates a left, 1:many, union join of the main data to child data
    { joinIngest :: Ingest
    , joinFirst -- ^parent field of first index
    , joinCount -- ^parent field of count
    , joinParent :: T.Text -- ^child field referencing parent
    }
  | IngestJoin -- ^Indicates a left, 1:0-or-1 join with additional dataset(s)
    { joinFields :: HM.HashMap T.Text Fields -- ^child ingest field to join on, and child ingest
    }

data Ingest = Ingest
  { ingestCatalog :: Catalog
  , ingestFile :: FilePath
  , ingestPrefix :: String -- ^prefix for document _ids (usually suffixed with record offset)
  , ingestConsts :: [FieldValue]
  , ingestJConsts :: J.Series -- ^cached rendered 'ingestConsts'
  , ingestBlockSize :: Word64
  , ingestOffset :: Word64 -- ^starting offset (skip) into file (without 'ingestStart')
  , ingestSize :: Maybe Word64 -- ^expected rows in this file
  , ingestStart :: Word64  -- ^base offset of this file (so first record is @'ingestStart' + 'ingestOffset'@)
  , ingestJoin :: Maybe IngestJoin
  }

addIngestConsts :: FieldValue -> Ingest -> Ingest
addIngestConsts v i = i{ ingestConsts = v : ingestConsts i, ingestJConsts = fieldJValue v <> ingestJConsts i }

fieldSource :: Field -> T.Text
fieldSource f = fromMaybe (fieldName f) $ fieldIngest f

ingestValueBS :: Field -> BS.ByteString -> Maybe J.Encoding
ingestValueBS f v
  | BS.null v
  || v `elem` fieldMissing f
  || typeIsFloating ft && v `elem` ["nan","NaN","Inf","-Inf","+Inf","inf"]
    = Nothing
  | Just i <- guard (not $ isDigit $ BSC.head v) >> fieldEnum f >>= V.elemIndex (TE.decodeLatin1 v)
    = Just $ J.toEncoding i
  | typeIsBoolean ft
    = Just $ J.toEncoding $ bool v
  | typeIsString ft
  || BSC.any isControl v
    = Just $ J.toEncoding $ str v
  {-
  | any typeIsBoolean (typeIsArray ft)
    = fieldName f `JE.pair` JE.unsafeToEncoding (B.byteString (BSC.map toLower v)) -- fix "[True,False]"
  -}
  | any typeIsFloating (typeIsArray ft)
    = Just $ JE.unsafeToEncoding (substitute "NaN" "-999999" v) -- XXX fix "[NaN]"
  | typeIsIntegral ft
    = Just $ JE.unsafeToEncoding (B.byteString $ drop0 v)
  | otherwise
    = Just $ JE.unsafeToEncoding (B.byteString v)
  where
  ft = fieldType f
  bool "0" = J.Bool False
  bool "0.0" = J.Bool False
  bool "false" = J.Bool False
  bool "False" = J.Bool False
  bool "1" = J.Bool True
  bool "1.0" = J.Bool True
  bool "true" = J.Bool True
  bool "True" = J.Bool True
  bool _ = str v
  str = J.String . TE.decodeLatin1
  substitute x y s = B.byteString p
    <> (if BS.null r then mempty else y <> substitute x y (BS.drop (BS.length x) r))
    where (p, r) = BS.breakSubstring x s
  drop0 s
    | BSC.null s' = "0"
    | otherwise = s'
    where s' = BSC.dropWhile ('0' ==) s

ingestFieldBS :: Field -> BS.ByteString -> J.Series
ingestFieldBS f v = foldMap (JK.fromText (fieldName f) `JE.pair`) $ ingestValueBS f v
