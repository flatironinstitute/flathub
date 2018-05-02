{-# LANGUAGE ViewPatterns #-}

module Ingest
  ( ingest
  ) where

import           Control.Arrow (first)
import           Data.Word (Word64)
import           System.FilePath (takeExtensions)
import           Text.Read (readMaybe)

import Schema
import Global
import Ingest.CSV
import Ingest.HDF5

ingest :: Catalog -> String -> M Word64
ingest cat fno =
  (case takeExtensions fn of
    ".hdf5" -> ingestHDF5
    '.':'c':'s':'v':_ -> ingestCSV
    _ -> error $ "Unknown ingest file type: " ++ fn)
    cat blockSize fn off
  where
  (fn, off) = splitoff fno
  splitoff [] = ([], 0)
  splitoff ('@':(readMaybe -> Just i)) = ([], i)
  splitoff (c:s) = first (c:) $ splitoff s
  blockSize = 1000
