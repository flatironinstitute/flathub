{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.Delim
  ( ingestDat
  , ingestTxt
  ) where

import           Control.Arrow (first)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import           Data.List (mapAccumL, findIndex, genericDrop, genericSplitAt)
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import           Data.Word (Word64)
import           Text.Read (readMaybe)
import           System.FilePath (dropExtension)
import           System.IO (hFlush, stdout)

import Global
import Field
import Catalog
import Compression
import qualified ES

data DelimHeaders
  = DelimHeaderComments
  | DelimHeaderLine
  deriving (Eq, Enum, Show)

data Delim = Delim
  { delimDelim :: !Char
  , delimMulti :: !Bool
  , delimHeaders :: !DelimHeaders
  } deriving (Show)

splitDelim :: Delim -> BSC.ByteString -> [BSC.ByteString]
splitDelim Delim{ delimDelim = c, delimMulti = False } =                           BSC.split c
splitDelim Delim{ delimDelim = c, delimMulti = True }  = filter (not . BSC.null) . BSC.split c

parseHeaders :: Delim -> [BSC.ByteString] -> ([BSC.ByteString], [BSC.ByteString])
parseHeaders _ [] = ([], [])
parseHeaders Delim{ delimHeaders = DelimHeaderComments } l = hdrs (0 :: Int) l where
  hdrs _ [] = ([], [])
  hdrs i hr@(h:r) = case BSC.words h of
    ("#":(readMaybe . BSC.unpack -> Just j):f:_) | j == i -> first (f :) $ hdrs (succ i) r
    _ -> ([], hr)
parseHeaders d@Delim{ delimHeaders = DelimHeaderLine } (h:r) = (splitDelim d h, r)

ingestDelim :: Delim -> Catalog -> J.Series -> Word64 -> FilePath -> Word64 -> M Word64
ingestDelim delim cat consts blockSize fn off = do
  ls <- liftIO $ map BSLC.toStrict . BSLC.lines <$> decompressFile fn
  let
    (header, rows) = parseHeaders delim ls
    (missing, fields) = mapAccumL (\fm s -> let n = TE.decodeUtf8 s in (HM.delete n fm, HM.lookup n fm)) (catalogFieldMap cat) header
    rows' = genericDrop off rows
    key
      | Just i <- do
          n <- catalogKey cat 
          findIndex (any ((n ==) . fieldName)) fields
        = \_ x -> BSC.unpack $ x !! i
      | otherwise = \i _ -> fnb ++ '_' : show i
    val Nothing _ = mempty
    val (Just f) x = fieldName f J..= TE.decodeLatin1 x
    loop o [] = return o
    loop o s = do
      liftIO $ putStr (show o ++ "\r") >> hFlush stdout
      let (d, s') = genericSplitAt blockSize s
          (o', block) = mapAccumL (\i l -> let x = splitDelim delim l in (succ i, (key i x, consts <> foldMap (uncurry val) (zip fields x)))) o d
      ES.createBulk cat block
      loop o' s'
  unless (HM.null missing) $ fail $ "missing fields: " ++ show (fieldName <$> missing)
  loop off rows'
  where
  fnb = dropExtension fn

ingestDat :: Catalog -> J.Series -> Word64 -> FilePath -> Word64 -> M Word64
ingestDat = ingestDelim (Delim ' ' False DelimHeaderComments)

ingestTxt :: Catalog -> J.Series -> Word64 -> FilePath -> Word64 -> M Word64
ingestTxt = ingestDelim (Delim ' ' True DelimHeaderComments)
