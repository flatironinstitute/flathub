{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Ingest
  ( ingest
  ) where

import           Control.Arrow (first)
import           Control.Monad (foldM)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (sort)
import           Data.Maybe (isJust, fromMaybe)
import           Data.Word (Word64)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath (takeExtension, (</>), takeBaseName)
import           Text.Read (readMaybe)

import Field
import Catalog
import Global
import Ingest.Types
import Ingest.CSV
import Ingest.Delim
import Ingest.HDF5
import Compression

ingest :: Catalog -> [FieldValue] -> [String] -> M Word64
ingest cat consts fs = do
  fs' <- liftIO $ concat <$> mapM expand fs
  foldM run 0 fs'
  where
  expand (splitoff -> fo@(f,o)) = do
    d <- doesDirectoryExist f
    if d
      then map ((, 0) . (f </>)) . drop (fromIntegral o) . sort . filter (isJust . proc) <$> listDirectory f
      else return [fo]
  run start (f, off) = do
    liftIO $ putStrLn f
    n <- ing f start off
    liftIO $ print n
    return (start + n)
  ing f start off = fromMaybe (error $ "Unknown ingest file type: " ++ f) (proc f)
    Ingest
      { ingestCatalog = cat
      , ingestFile = f
      , ingestPrefix = takeBaseName f <> "_"
      , ingestConsts = consts
      , ingestBlockSize = 1000
      , ingestStart = start
      , ingestOffset = off
      , ingestSize = Nothing
      }
  proc f = case takeExtension $ fst $ decompressExtension f of
    ".hdf5" -> Just ingestHDF5
    ".csv" -> Just ingestCSV
    ".dat" -> Just ingestDat
    ".txt" -> Just ingestTxt
    _ -> Nothing
  splitoff [] = ([], 0)
  splitoff ('@':(readMaybe -> Just i)) = ([], i)
  splitoff (c:s) = first (c:) $ splitoff s
