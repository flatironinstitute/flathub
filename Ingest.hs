{-# LANGUAGE ViewPatterns #-}

module Ingest
  ( ingest
  ) where

import           Control.Arrow (first)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (sort)
import           Data.Maybe (isJust, fromMaybe)
import           Data.Word (Word64)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath (takeExtension, splitExtension)
import           Text.Read (readMaybe)

import Schema
import Global
import Ingest.CSV
import Ingest.HDF5

ingest :: Catalog -> String -> M Word64
ingest cat fno = do
  d <- liftIO $ doesDirectoryExist fn
  if d
    then do
      l <- liftIO $ drop (fromIntegral off) . sort . filter (isJust . proc) <$> listDirectory fn
      sum <$> mapM (\f -> do
        liftIO $ putStrLn f
        ing f 0) l
    else ing fn off
  where
  ing f = fromMaybe (error $ "Unknown ingest file type: " ++ f) (proc f)
    cat blockSize f
  proc f = case takeExtension $ dropz f of
    ".hdf5" -> Just ingestHDF5
    ".csv" -> Just ingestCSV
    _ -> Nothing
  dropz f = case splitExtension fn of
    (b, ".gz") -> b
    (b, ".bz2") -> b
    _ -> f
  (fn, off) = splitoff fno
  splitoff [] = ([], 0)
  splitoff ('@':(readMaybe -> Just i)) = ([], i)
  splitoff (c:s) = first (c:) $ splitoff s
  blockSize = 1000
