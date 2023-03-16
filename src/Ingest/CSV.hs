{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.CSV
  ( unconsCSV
  , ingestCSVFrom
  , ingestCSV
  ) where

import           Control.Arrow (first)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Csv.Streaming as CSV
import           Data.List (mapAccumL)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word64)
import           System.IO (hFlush, stdout)

import Field
import Catalog
import Error
import Global
import qualified ES
import Compression
import Ingest.Types

dropCSV :: Word64 -> CSV.Records a -> (Word64, CSV.Records a)
dropCSV n (CSV.Cons _ r) | n > 0 = dropCSV (pred n) r
dropCSV n r = (n, r)

unconsCSV :: CSV.Records a -> Err (Maybe a, CSV.Records a)
unconsCSV (CSV.Cons h r) = (, r) . Just <$> either raise400 return h
unconsCSV n@(CSV.Nil e _) = (, n) <$> mapM raise400 e

takeCSV :: Word64 -> CSV.Records a -> Err ([a], CSV.Records a)
takeCSV 0 r = return ([], r)
takeCSV n r = do
  (a, r') <- unconsCSV r
  maybe
    (return ([], r')) 
    (\a' -> first (a':) <$> takeCSV (pred n) r')
    a

ingestCSVFrom :: Ingest -> V.Vector BS.ByteString -> CSV.Records (V.Vector BS.ByteString) -> M Word64
ingestCSVFrom info@Ingest{ ingestCatalog = cat, ingestOffset = off } header rows = do
  cols <- mapM (\f -> (,) f <$>
      maybe (if fieldSource f == "_index" then return (-1) else
        raise400 $ "csv header field missing: " ++ T.unpack (fieldName f))
        return
        (V.elemIndex (TE.encodeUtf8 $ fieldSource f) header))
    $ catalogFields cat
  let
    (del, rows') = dropCSV off rows
    off' = off - del
    key
      | Just (_, k) <- (\n -> V.find ((n ==) . fieldName . fst) cols) =<< catalogKey cat = const $ BSC.unpack . (V.! k)
      | otherwise = const . (ingestPrefix info ++) . show
    val o _ (f, -1) = JK.fromText (fieldName f) J..= o
    val _ r (f, i) = ingestFieldBS f (r V.! i)
    loop o cs = do
      liftIO $ putStr (show o ++ "\r") >> hFlush stdout
      (rs, cs') <- runErr $ takeCSV (ingestBlockSize info) cs
      if null rs
        then return o
        else do
          let (o', block) = mapAccumL (\i r -> (succ i, (key i r, ingestJConsts info <> foldMap (val i r) cols))) o rs
          ES.createBulk cat block
          loop o' cs'
  loop off' rows'

ingestCSV :: Ingest -> M Word64
ingestCSV info = do
  dat <- liftIO $ decompressFile $ ingestFile info
  let csv = CSV.decode CSV.NoHeader dat
  (fromMaybe V.empty -> header, rows) <- runErr $ unconsCSV csv
  ingestCSVFrom info header rows
