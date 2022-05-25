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
import           Data.List (stripPrefix, mapAccumL)
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
  { tableName :: T.Text
  , tableKeyIndex :: Int
  , tableFields :: Fields
  , tableFieldIndex :: V.Vector Int
  }

data TableStream = TableStream
  { tableNext :: Maybe (Key, J.Series)
  , tableRows :: CSV.Records (V.Vector BS.ByteString)
  , tableInfo :: TableInfo
  }

nextRow :: TableStream -> TableStream
nextRow ts@TableStream{..} = ts
  { tableNext = mk <$> next
  , tableRows = rows
  }
  where
  mk row = 
    ( maybe (error "key read") fst $ mfilter (BS.null . snd) $ BSC.readInt (row V.! tableKeyIndex tableInfo)
    , fold $ V.zipWith (\f -> ingestFieldBS f . (row V.!)) (tableFields tableInfo) (tableFieldIndex tableInfo))
  (next, rows) = errorErr $ unconsCSV tableRows

readTable :: FilePath -> Field -> (Int, Int) -> T.Text -> Fields -> IO TableStream
readTable path keyf range name fields = do
  (hd, rows) <- loadECSV file fields
  let geti n = maybe (fail $ file ++ " missing field " ++ BSC.unpack n) return $ V.elemIndex n hd
  key <- geti (fieldsrc keyf)
  cols <- mapM (geti . fieldsrc) fields
  return $ nextRow TableStream
    { tableNext = Nothing
    , tableRows = rows
    , tableInfo = TableInfo
      { tableName = name
      , tableFields = fields
      , tableKeyIndex = key
      , tableFieldIndex = cols
      }
    }
  where
  file = joinFile path (T.unpack name) range
  fieldsrc = TE.encodeUtf8 . fieldSource

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

nextKey :: Key -> J.Series -> TableStream -> (J.Series, TableStream)
nextKey k b ts@TableStream{ tableNext = Just (tk, r) } = case compare k tk of
  LT -> (mempty, ts)
  EQ -> (b <> r, nextRow ts)
  GT -> error $ T.unpack (tableName (tableInfo ts)) ++ " out of order at " ++ show tk ++ " > " ++ show k
nextKey _ b ts@TableStream{ tableNext = Nothing } = (b, ts)

nextKeys :: Key -> J.Series -> [TableStream] -> (J.Series, [TableStream])
nextKeys = mapAccumL . nextKey

ingestGaiaDR3 :: Ingest -> M Word64
ingestGaiaDR3 ing@Ingest{ ingestCatalog = cat } = do
  (keytab, keyf) <- split <$> lookupField cat True keyn
  unless (T.unpack keytab == maintabn) $ fail "file path doesn't match key table"
  tabmap <- liftIO $ HM.traverseWithKey (readTable basedir keyf range) tabfs
  let maintab = tabmap HM.! keytab
      tabs = HM.elems $ HM.delete keytab tabmap
      loop :: TableStream -> [TableStream] -> M Word64
      loop m@TableStream{ tableNext = Just (k, r) } tl = do
        liftIO $ print $ J.pairs r'
        loop m' tl'
        where
        (r', tl') = nextKeys k r tl
        m' = nextRow m
      loop TableStream{ tableNext = Nothing } tl = do
        mapM_ (\t -> mapM_ (\(k, _) -> fail $ "leftover in " ++ T.unpack (tableName (tableInfo t)) ++ ": " ++ show k) $ tableNext t) tl
        return 0
  loop maintab tabs
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
