{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Ingest.GaiaDR3
  ( ingestGaiaDR3
  ) where

import           Control.Arrow (second)
import           Control.Monad (mfilter, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import           Data.Bits (shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Csv.Streaming as CSV
import           Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word64)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import qualified System.FilePath as FP

import Error
import Field
import Catalog
import Global
import Ingest.Types
import Ingest.ECSV
import Ingest.CSV

type Key = Int

indexLevel :: Int
indexLevel = 59 - 2*level where
  level = 8

-- |convert from source_id to file naming, HEALpix index level 8
keyIndex :: Key -> Int
keyIndex = (`shiftR` indexLevel)

indexKey :: Int -> Key
indexKey = (`shiftL` indexLevel)

keyInRange :: (Int, Int) -> Key -> Bool
keyInRange (x, y) = \i -> i >= indexKey x && i < indexKey (succ y)

data TableInfo = TableInfo
  { tableKeyIndex :: Int
  , tableFields :: Fields
  , tableFieldIndex :: V.Vector Int
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
    (return ts{ tableNext = Nothing })
    (\row -> do
      k <- maybe (fail "key read") (return . fst) $ mfilter (BS.null . snd) $ BSC.readInt (row V.! tableKeyIndex tableInfo)
      return ts
        { tableNext = Just (k, fold $ V.zipWith (\f -> ingestFieldBS f . (row V.!)) (tableFields tableInfo) (tableFieldIndex tableInfo))
        , tableRows = rows
        })
    next

loadFile :: Field -> Fields -> FilePath -> IO TableStream
loadFile keyf fields file = do
  (hd, rows) <- loadECSV file fields
  let geti n = maybe (fail $ file ++ " missing field " ++ BSC.unpack n) return $ V.elemIndex n hd
  key <- geti (fieldsrc keyf)
  cols <- mapM (geti . fieldsrc) fields
  nextRow TableStream
    { tableNext = Nothing
    , tableRows = rows
    , tableInfo = TableInfo
      { tableFields = fields
      , tableKeyIndex = key
      , tableFieldIndex = cols
      }
    }
  where
  fieldsrc = TE.encodeUtf8 . fieldSource

readTable :: FilePath -> Field -> (Int, Int) -> T.Text -> Fields -> IO TableStream
readTable path keyf range name fields =
  loadFile keyf fields $ joinFile path name' range
  where
  name' = T.unpack name

-- |Split a path like /foo/bar/GaiaSource/GaiaSource_123456-789012.csv into @("/foo/bar/", "GaiaSource", 123456, 789012)@
splitFile :: FilePath -> (FilePath, String, (Int, Int))
splitFile fp = fromMaybe (error $ "could not parse file path: " ++ fp) $ do
  let (bd, dn) = FP.splitFileName $ FP.takeDirectory fp
  b <- stripPrefix (dn ++ "_") =<< FP.stripExtension "csv" (FP.takeFileName fp)
  let (is, '-':js) = break ('-' ==) b
  i <- readMaybe is
  j <- readMaybe js
  return (bd, dn, (i, j))

-- |Inverse of 'splitFile'
joinFile :: FilePath -> String -> (Int, Int) -> FilePath
joinFile bd dn (i, j) = bd FP.</> dn FP.</> (dn ++ "_" ++ r i ++ "-" ++ r j) FP.<.> "csv" where
  r = printf "%06d"

ingestGaiaDR3 :: Ingest -> M Word64
ingestGaiaDR3 ing@Ingest{ ingestCatalog = cat } = do
  (keytab, keyf) <- split <$> lookupField cat True keyn
  unless (T.unpack keytab == maintabn) $ fail "file path doesn't match key table"
  tabs <- liftIO $ HM.traverseWithKey (readTable basedir keyf range) tabfs
  return 0
  where
  (basedir, maintabn, range) = splitFile (ingestFile ing)
  inrange = keyInRange range
  fields = catalogFields cat
  Just keyn = catalogKey cat
  tabfs = HM.map V.fromList $ V.foldr (uncurry (HM.insertWith (++)) . second return . split) HM.empty fields
  split f@Field{ fieldDesc = fd@FieldDesc{ fieldDescIngest = Just i } } =
    second (\n -> f{ fieldDesc = fd{ fieldDescIngest = snd <$> T.uncons n } })
      $ T.breakOn "." i
  split f = error $ "field missing ingest: " ++ T.unpack (fieldName f)
