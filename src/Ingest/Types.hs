{-# LANGUAGE OverloadedStrings #-}
module Ingest.Types
  where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isControl)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word64)

import Type
import Field
import Catalog

-- Indicates a (left, 1:many) join of the main data to child data
data IngestJoin = IngestJoin
  { joinIngest :: Ingest
  , joinFirst -- ^parent field of first index
  , joinCount -- ^parent field of count
  , joinParent :: T.Text -- ^child field referencing parent
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

ingestFieldBS :: Field -> BS.ByteString -> J.Series
ingestFieldBS f v
  | BS.null v
  || v `elem` fieldMissing f
  || typeIsFloating ft && v `elem` ["nan","NaN","Inf","-Inf","+Inf","inf"]
    = mempty
  | typeIsBoolean ft
    = fieldName f J..= bool v
  | typeIsString ft
  || BSC.any isControl v
    = fieldName f J..= str v
  {-
  | any typeIsBoolean (typeIsArray ft)
    = fieldName f `JE.pair` JE.unsafeToEncoding (B.byteString (BSC.map toLower v)) -- fix "[True,False]"
  | any typeIsFloating (typeIsArray ft)
    = fieldName f `JE.pair` JE.unsafeToEncoding (substitute "NaN" "null" v) -- fix "[NaN]"
  -}
  | otherwise
    = fieldName f `JE.pair` JE.unsafeToEncoding (B.byteString v)
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
  _substitute x y s = B.byteString p
    <> (if BS.null r then mempty else y <> _substitute x y (BS.drop (BS.length x) r))
    where (p, r) = BS.breakSubstring x s
