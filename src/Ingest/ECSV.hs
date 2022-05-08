{-# LANGUAGE OverloadedStrings #-}

module Ingest.ECSV
  ( ingestECSV
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Csv.Streaming as CSV
import           Data.Function (on)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word (Word64)
import qualified Data.Vector as V

import Data.ECSV
import Field
import Catalog
import Global
import Compression
import Ingest.Types
import Ingest.CSV
import Output.ECSV (ecsvField)

ingestECSV :: Ingest -> M Word64
ingestECSV info@Ingest{ ingestCatalog = cat } = do
  dat <- liftIO $ decompressFile $ ingestFile info
  let (ehead, body) = parseECSVHeader dat
      csv = CSV.decode CSV.NoHeader body
  ecsv <- either (raise400 . show) return ehead
  V.zipWithM_ (\n f -> do
    let t = ecsvField f
    e <- maybe (raise400 $ "ECSV column missing: " ++ T.unpack n) return
      $ V.find ((n ==) . ecsvColName) (ecsvDatatype ecsv)
    unless (on (==) (ecsvColDataType &&& ecsvColSubtype) e t)
      $ raise400 $ "ECSV column type mismatch: " ++ T.unpack n)
    fn (catalogFields cat)
  (Just csvh, _) <- runErr $ unconsCSV csv
  unless (csvh == V.map (TE.encodeUtf8 . ecsvColName) (ecsvDatatype ecsv)) $
    raise400 "ECSV/CSV header mismatch"
  ingestCSVFrom info csv
  where
  fn = V.map (\f -> fromMaybe (fieldName f) (fieldIngest f)) $ catalogFields cat
