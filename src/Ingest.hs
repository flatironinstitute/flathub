{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest
  ( ingest
  ) where

import           Control.Arrow (first)
import           Control.Monad (foldM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           System.FilePath (takeExtension, (</>), takeBaseName)
import           Text.Read (readMaybe)

import Field
import Catalog
import Global
import Ingest.Types
import Ingest.CSV
import Ingest.ECSV
import Ingest.Delim
import Ingest.HDF5
import Ingest.GaiaDR3
import Compression

ingest :: Catalog -> [FieldValue] -> [String] -> M Word64
ingest cat consts fs = do
  base <- asks globalDataDir
  foldM run 0 $ map (expand base) fs
  where
  expand base (splitoff -> (splitpfx -> (p,f'),o)) = (p,base </> f',o)
  run start (p, f, off) = do
    liftIO $ putStrLn $ f <> " [" <> p <> "]"
    n <- ing f p start off
    liftIO $ print n
    return (start + n)
  ing f pfx start off = fromMaybe (error $ "Unknown ingest file type: " ++ f) (proc f)
    Ingest
      { ingestCatalog = cat
      , ingestFile = f
      , ingestPrefix = (if null pfx then id else (<> "_")) pfx
      , ingestConsts = consts
      , ingestJConsts = fieldJValues consts
      , ingestBlockSize = 1000
      , ingestStart = start
      , ingestOffset = off
      , ingestSize = Nothing
      , ingestJoin = Nothing
      }
  proc f = case takeExtension $ fst $ decompressExtension f of
    ".hdf5" -> Just ingestHDF5
    ".h5" -> Just ingestHDF5
    ".csv"
      | catalogName cat == "gaiadr3" -> Just ingestGaiaDR3
      | otherwise -> Just ingestCSV
    ".ecsv" -> Just ingestECSV
    ".dat" -> Just ingestDat
    ".txt" -> Just ingestTxt
    "" | catalogName cat == "tng" -> Just ingestTNG
    _ -> Nothing
  splitoff [] = ([], 0)
  splitoff ('@':(readMaybe -> Just i)) = ([], i)
  splitoff (c:s) = first (c:) $ splitoff s
  splitpfx s = case break ('=' ==) s of
    (r, []) -> (takeBaseName r, r)
    (p, ~('=':r)) -> (p, r)
