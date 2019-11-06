module Ingest.Types
  where

import qualified Data.Aeson as J
import           Data.Word (Word64)
import           System.FilePath (FilePath)

import Catalog
import Field

data Ingest = Ingest
  { ingestCatalog :: Catalog
  , ingestFile :: FilePath
  , ingestPrefix :: String -- ^prefix for document _ids (usually suffixed with record offset)
  , ingestConsts :: [FieldValue]
  , ingestBlockSize :: Word64
  , ingestOffset :: Word64 -- ^starting offset (skip) into file (without 'ingestStart')
  , ingestSize :: Maybe Word64 -- ^expected rows in this file
  , ingestStart :: Word64  -- ^base offset of this file (so first record is @'ingestStart' + 'ingestOffset'@)
  }

ingestJConsts :: Ingest -> J.Series
ingestJConsts = foldMap (\f -> fieldName f J..= fieldType f) . ingestConsts
