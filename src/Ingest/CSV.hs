{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.CSV
  ( ingestCSV
  ) where

import           Control.Arrow (first)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Csv.Streaming as CSV
import           Data.List (mapAccumL)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word64)
import           System.IO (hFlush, stdout)

import Monoid
import Field
import Catalog
import Global
import qualified ES
import Compression
import Ingest.Types

dropCSV :: Word64 -> CSV.Records a -> (Word64, CSV.Records a)
dropCSV n (CSV.Cons _ r) | n > 0 = dropCSV (pred n) r
dropCSV n r = (n, r)

unconsCSV :: MonadFail m => CSV.Records a -> m (Maybe a, CSV.Records a)
unconsCSV (CSV.Cons h r) = (, r) . Just <$> either fail return h
unconsCSV n@(CSV.Nil e _) = (, n) <$> mapM fail e

takeCSV :: MonadFail m => Word64 -> CSV.Records a -> m ([a], CSV.Records a)
takeCSV 0 r = return ([], r)
takeCSV n r = do
  (a, r') <- unconsCSV r
  maybe
    (return ([], r')) 
    (\a' -> first (a':) <$> takeCSV (pred n) r')
    a

ingestCSV :: Ingest -> M Word64
ingestCSV info@Ingest{ ingestCatalog = cat, ingestOffset = off } = do
  csv <- liftIO $ CSV.decode CSV.NoHeader <$> decompressFile (ingestFile info)
  (fromMaybe V.empty -> header, rows) <- unconsCSV csv
  cols <- mapM (\Field{ fieldName = n, fieldIngest = i } ->
      maybe (fail $ "csv header field missing: " ++ T.unpack n) (return . (,) n) $ V.elemIndex (fromMaybe n i) header)
    $ catalogFields cat
  let
    (del, rows') = dropCSV off rows
    off' = off - del
    key
      | Just k <- (`lookup` cols) =<< catalogKey cat = const $ T.unpack . (V.! k)
      | otherwise = const . (ingestPrefix info ++) . show
    val r (n, i) = mwhen (not $ T.null v) (n J..= v)
      where v = r V.! i
    loop o cs = do
      liftIO $ putStr (show o ++ "\r") >> hFlush stdout
      (rs, cs') <- takeCSV (ingestBlockSize info) cs
      if null rs
        then return o
        else do
          let (o', block) = mapAccumL (\i r -> (succ i, (key i r, ingestJConsts info <> foldMap (val r) cols))) o rs
          ES.createBulk cat block
          loop o' cs'
  loop off' rows'
