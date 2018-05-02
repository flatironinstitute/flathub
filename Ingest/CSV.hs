{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.CSV
  ( ingestCSV
  ) where

import qualified Codec.Compression.BZip as BZ
import qualified Codec.Compression.GZip as GZ
import           Control.Arrow (first)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Csv.Streaming as CSV
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word64)
import           System.FilePath (splitExtensions)

import Schema
import Global

dropCSV :: Word64 -> CSV.Records a -> (Word64, CSV.Records a)
dropCSV 0 r = (0, r)
dropCSV n (CSV.Cons _ r) = dropCSV (pred n) r
dropCSV n r = (n, r)

unconsCSV :: Monad m => CSV.Records a -> m (Maybe a, CSV.Records a)
unconsCSV (CSV.Cons h r) = (, r) . Just <$> either fail return h
unconsCSV n@(CSV.Nil e _) = (, n) <$> mapM fail e

takeCSV :: Monad m => Word64 -> CSV.Records a -> m ([a], CSV.Records a)
takeCSV 0 r = return ([], r)
takeCSV n r = do
  (a, r') <- unconsCSV r
  maybe
    (return ([], r')) 
    (\a' -> first (a':) <$> takeCSV (pred n) r')
    a

ingestCSV :: Catalog -> Word64 -> FilePath -> Word64 -> M Word64
ingestCSV cat blockSize fn off = do
  csv <- liftIO $ CSV.decode CSV.NoHeader . (case fne of
      ".csv.gz" -> GZ.decompress
      ".csv.bz2" -> BZ.decompress
      ".csv" -> id
      _ -> error "Unspported CSV file")
    <$> BSLC.readFile fn
  (fromMaybe V.empty -> header, rows) <- unconsCSV csv
  cols <- mapM (\Field{ fieldName = n } ->
      maybe (fail $ "csv header field missing: " ++ T.unpack n) (return . (,) n) $ V.elemIndex n header)
    $ catalogFields cat
  let (off', rows') = dropCSV off rows
  return off'
  where
  (fnb, fne) = splitExtensions fn
  loop csv = do
    (rows, csv') <- takeCSV blockSize csv
    return ()
