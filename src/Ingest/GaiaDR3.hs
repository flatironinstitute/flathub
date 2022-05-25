{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Ingest.GaiaDR3
  ( ingestGaiaDR3
  ) where

import           Control.Arrow (second)
import           Control.Monad (mfilter)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv.Streaming as CSV
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
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
  { tableFields :: Fields
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
  (hd, rows) <- loadECSV file (tableFields ti)
  let geti n = maybe (fail $ file ++ " missing field " ++ BSC.unpack n) return $ V.elemIndex n hd
  key <- geti (tableKeyName ti)
  cols <- mapM (\f -> geti (TE.encodeUtf8 $ fromMaybe (fieldName f) (mfilter (not . T.isPrefixOf "_") $ fieldIngest f)))
    $ tableFields ti
  nextRow TableStream
    { tableNext = Nothing
    , tableRows = rows
    , tableInfo = ti
      { tableKeyIndex = key
      , tableFieldIndex = cols
      , tableFiles = files
      }
    }

readTable :: Field -> FilePath -> T.Text -> Fields -> IO TableStream
readTable keyf path name fields = do
  files <- map (dir </>) . sort . filter match <$> listDirectory dir
  nextFile $ TableInfo
    { tableFields = fields
    , tableKeyName = TE.encodeUtf8 $ fieldSource keyf
    , tableKeyIndex = -1
    , tableFieldIndex = V.empty
    , tableFiles = files
    }
  where
  name' = T.unpack name
  dir = path </> name'
  match f = (name' ++ "_") `isPrefixOf` f && ".csv" `isExtensionOf` f

fieldSource :: Field -> T.Text
fieldSource f = fromMaybe (fieldName f) $ fieldIngest f

ingestGaiaDR3 :: Ingest -> M Word64
ingestGaiaDR3 ing = do
  tabs <- liftIO $ HM.traverseWithKey (readTable keyf (ingestFile ing)) tabfs
  return 0
  where
  fields = catalogFields $ ingestCatalog ing
  (keytab, keyf) = split $ fromMaybe (error "no _key")
    $ V.find (any (T.isPrefixOf flagKey) . fieldIngest) fields
  tabfs = HM.map V.fromList $ V.foldr (uncurry (HM.insertWith (++)) . second return . split) HM.empty fields
  split f@Field{ fieldDesc = fd@FieldDesc{ fieldDescIngest = Just i } } =
    second (\n -> f{ fieldDesc = fd{ fieldDescIngest = snd <$> T.uncons n } })
      $ T.breakOn "." $ fromMaybe i (T.stripPrefix flagKey i)
  split f = error $ "field missing ingest: " ++ T.unpack (fieldName f)
  flagKey = "_key:"
