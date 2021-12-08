module Ingest.Types
  where

import qualified Data.Aeson as J
import qualified Data.Text as T
import           Data.Word (Word64)

import Catalog
import Field

-- Indicates a (left, 1:many) join of the main data to child data
data IngestJoin = IngestJoin
  { joinIngest :: Ingest
  , joinFirst -- ^parent field of first index
  , joinCount -- ^parent field of count
  , joinParent :: T.Text -- ^child field referencing parent
  }

data Ingest = Ingest
  { ingestCatalog :: Catalog
  , ingestFile :: FilePath
  , ingestPrefix :: String -- ^prefix for document _ids (usually suffixed with record offset)
  , ingestConsts :: [FieldValue]
  , ingestJConsts :: J.Series -- ^cached rendered 'ingestConsts'
  , ingestBlockSize :: Word64
  , ingestOffset :: Word64 -- ^starting offset (skip) into file (without 'ingestStart')
  , ingestSize :: Maybe Word64 -- ^expected rows in this file
  , ingestStart :: Word64  -- ^base offset of this file (so first record is @'ingestStart' + 'ingestOffset'@)
  , ingestJoin :: Maybe IngestJoin
  }

addIngestConsts :: FieldValue -> Ingest -> Ingest
addIngestConsts v i = i{ ingestConsts = v : ingestConsts i, ingestJConsts = fieldJValue v <> ingestJConsts i }
