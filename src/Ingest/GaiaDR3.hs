{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Ingest.GaiaDR3
  ( ingestGaiaDR3
  ) where

import           Control.Monad (mfilter)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv.Streaming as CSV
import           Data.Foldable (fold)
import           Data.List (isPrefixOf, sort)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word64)
import           System.Directory (listDirectory)
import           System.FilePath ((</>), isExtensionOf)

import Error
import Field
import Catalog
import Global
import Ingest.Types
import Ingest.ECSV
import Ingest.CSV

type Key = Int

data TableInfo = TableInfo
  { tableSubFields
  , tableFields :: Fields
  , tableKeyName :: BS.ByteString
  , tableKeyIndex :: Int
  , tableFieldIndex :: V.Vector Int
  , tableFiles :: [FilePath]
  }

data TableStream = TableStream
  { tableNext :: Maybe (Key, J.Series)
  , tableRows :: CSV.Records (V.Vector BS.ByteString)
  , tableInfo :: TableInfo
  }

nextRow :: TableStream -> IO TableStream
nextRow ts@TableStream{..} = do
  (next, rows) <- failErr $ unconsCSV tableRows
  maybe
    (nextFile tableInfo)
    (\row -> do
      k <- maybe (fail "key read") (return . fst) $ mfilter (BS.null . snd) $ BSC.readInt (row V.! tableKeyIndex tableInfo)
      return ts
        { tableNext = Just (k, fold $ V.zipWith (\f -> ingestFieldBS f . (row V.!)) (tableFields tableInfo) (tableFieldIndex tableInfo))
        , tableRows = rows
        })
    next

nextFile :: TableInfo -> IO TableStream
nextFile ti@TableInfo{ tableFiles = [] } = return $ TableStream Nothing (CSV.Nil Nothing BSL.empty) ti
nextFile ti@TableInfo{ tableFiles = file:files } = do
  (hd, rows) <- loadECSV file (tableSubFields ti)
  let geti n = maybe (fail $ file ++ " missing field " ++ BSC.unpack n) return $ V.elemIndex n hd
  key <- geti (tableKeyName ti)
  cols <- mapM (\f -> geti (TE.encodeUtf8 $ fromMaybe (fieldName f) (mfilter (not . T.isPrefixOf "_") $ fieldIngest f)))
    $ tableSubFields ti
  nextRow TableStream
    { tableNext = Nothing
    , tableRows = rows
    , tableInfo = ti
      { tableKeyIndex = key
      , tableFieldIndex = cols
      , tableFiles = files
      }
    }

readTable :: T.Text -> FilePath -> T.Text -> FieldGroup -> FieldGroups -> IO TableStream
readTable key path name base fields = do
  files <- map (dir </>) . sort . filter match <$> listDirectory dir
  nextFile $ TableInfo
    { tableSubFields = fields'
    , tableFields = V.map (subField base) fields'
    , tableKeyName = TE.encodeUtf8 key
    , tableKeyIndex = -1
    , tableFieldIndex = V.empty
    , tableFiles = files
    }
  where
  fields' = expandFields fields
  name' = T.unpack name
  dir = path </> name'
  match f = (name' ++ "_") `isPrefixOf` f && ".csv" `isExtensionOf` f

ingestGaiaDR3 :: Ingest -> M Word64
ingestGaiaDR3 ing = do
  maintab <- readtab keytab mempty fields
  subtabs <- V.mapM readsub tables
  return 0
  where
  (tables, fields) = V.partition (any (T.isPrefixOf flagTable) . fieldIngest) $ catalogFieldGroups $ ingestCatalog ing
  Just keyf = V.find (any (T.isPrefixOf flagKey) . fieldIngest) fields
  Just keytab = T.stripPrefix flagKey =<< fieldIngest keyf
  keyn = fieldName keyf
  readtab n b = liftIO . readTable keyn (ingestFile ing) n b
  readsub f = fromMaybe (fail "no table sub") $
    readtab <$> (T.stripPrefix flagTable =<< fieldIngest f) <*> return f <*> fieldSub f
  flagKey = "_key:"
  flagTable = "_table:"
