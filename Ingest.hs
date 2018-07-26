{-# LANGUAGE ViewPatterns #-}

module Ingest
  ( ingest
  ) where

import           Control.Arrow (first)
import           Control.Monad (foldM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import           Data.List (sort)
import           Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T
import           Data.Word (Word64)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath (takeExtension, (</>))
import           Text.Read (readMaybe)

import Catalog
import Global
import Ingest.CSV
import Ingest.Delim
import Ingest.HDF5
import Compression

ingest :: Catalog -> [(T.Text, T.Text)] -> String -> M Word64
ingest cat consts fno = do
  d <- liftIO $ doesDirectoryExist fn
  if d
    then foldM (\i f -> do
        liftIO $ putStrLn (fn </> f)
        (i +) <$> ing (fn </> f) 0) 0
      . drop (fromIntegral off) . sort . filter (isJust . proc)
      =<< liftIO (listDirectory fn)
    else ing fn off
  where
  ing f = fromMaybe (error $ "Unknown ingest file type: " ++ f) (proc f)
    cat jconsts blockSize f
  proc f = case takeExtension $ fst $ decompressExtension f of
    ".hdf5" -> Just ingestHDF5
    ".csv" -> Just ingestCSV
    ".dat" -> Just ingestDat
    _ -> Nothing
  (fn, off) = splitoff fno
  splitoff [] = ([], 0)
  splitoff ('@':(readMaybe -> Just i)) = ([], i)
  splitoff (c:s) = first (c:) $ splitoff s
  blockSize = 1000
  jconsts = foldMap (uncurry (J..=)) consts
