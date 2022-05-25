{-# LANGUAGE OverloadedStrings #-}

module Ingest.ECSV
  ( loadECSV
  , ingestECSV
  ) where

import           Control.Arrow ((&&&), first)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
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
import Error
import Global
import Compression
import Ingest.Types
import Ingest.CSV
import Output.ECSV (ecsvField)

checkType :: Field -> ECSVColumn -> ECSVColumn -> Bool
checkType _ _ ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Nothing } = True -- allow string for any type
checkType _ e c = (on (==) (ecsvColDataType &&& ecsvColSubtype) e c)

loadECSV :: FilePath -> Fields -> IO (V.Vector BS.ByteString, CSV.Records (V.Vector BS.ByteString))
loadECSV file fields = do
  dat <- liftIO $ decompressFile file
  let (ehead, body) = parseECSVHeader dat
      csv = CSV.decode CSV.NoHeader body
  ecsv <- either (fail . show) return ehead
  V.zipWithM_ (\n f -> do
    let e = ecsvField f
    c <- maybe (fail $ "ECSV column missing: " ++ T.unpack n) return
      $ V.find ((n ==) . ecsvColName) (ecsvDatatype ecsv)
    unless (checkType f e c)
      $ fail $ "ECSV column type mismatch: " ++ T.unpack n)
    fn fields
  csvhr@(csvh, _) <- failErr $ first (fromMaybe V.empty) <$> unconsCSV csv
  unless (csvh == V.map (TE.encodeUtf8 . ecsvColName) (ecsvDatatype ecsv)) $
    fail "ECSV/CSV header mismatch"
  return csvhr
  where
  fn = V.map fieldSource fields

ingestECSV :: Ingest -> M Word64
ingestECSV info@Ingest{ ingestCatalog = cat } = do
  csv <- liftIO $ loadECSV (ingestFile info) (catalogFields cat)
  uncurry (ingestCSVFrom info) csv
