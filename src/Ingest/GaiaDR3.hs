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
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Key as JK
import           Data.Bits (shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Csv.Streaming as CSV
import           Data.Foldable (foldlM)
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.List (stripPrefix, mapAccumL, genericDrop, sortOn, groupBy)
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word (Word64)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import           System.Directory (doesPathExist)
import qualified System.FilePath as FP
import           System.IO (hFlush, hPutStrLn, stdout, stderr)

import Field
import Catalog
import Global
import Ingest.Types
import Ingest.ECSV
import qualified ES

type Key = Int64

indexLevel :: Int
indexLevel = 59 - 2*level where level = 8

-- |convert from source_id to file naming, HEALpix index level 8
keyIndex :: Key -> Int
keyIndex = fromIntegral . (`shiftR` indexLevel)

indexKey :: Int -> Key
indexKey = (`shiftL` indexLevel) . fromIntegral

keyInRange :: (Int, Int) -> Key -> Bool
keyInRange (x, y) = \i -> i >= indexKey x && i < indexKey (succ y)

data Row a = Row
  { rowIndex :: !Word
  , rowKey :: !Key
  , rowData :: !a
  }

instance Functor Row where
  fmap f r = r{ rowData = f (rowData r) }

data TableInfo = TableInfo
  { tableName :: T.Text
  , tableKeyIndex :: Int
  , tableFields :: V.Vector (Field, Int)
  }

data Table = Table
  { tableInfo :: TableInfo
  , tableRows :: [Row J.Series]
  }

parseRows :: TableInfo -> CSV.Records (V.Vector BS.ByteString) -> [Row (V.Vector BS.ByteString)]
parseRows TableInfo{..} = rows 0 where
  key r = maybe (error "key read") (fromInteger . fst) $ mfilter (BS.null . snd) $ BSC.readInteger (r V.! tableKeyIndex)
  row i d = Row i (key d) d
  rows i (CSV.Cons h r) = either error (row i) h : rows (succ i) r
  rows _ (CSV.Nil e _) = maybe [] error e

parseTableDups :: TableInfo -> [Row (V.Vector BS.ByteString)] -> Table
parseTableDups ti@TableInfo{..} = Table ti . map row . groupBy ((==) `on` rowKey) where
  row ~l@(r:_) =
    r{ rowData = V.foldMap (\(f, i) -> JK.fromText (fieldName f) `JE.pair` JE.list (cell f i) l) tableFields } where
    cell _ (-1) Row{ rowIndex = idx } = J.toEncoding idx
    cell f i Row{ rowData = d } = fromMaybe (J.toEncoding J.Null) $ ingestValueBS f (d V.! i)

parseTable :: TableInfo -> [Row (V.Vector BS.ByteString)] -> Table
parseTable ti@TableInfo{..} = Table ti . map row where
  row r@(Row idx _ d) =
    r{ rowData = V.foldMap cell tableFields } where
    cell (f,-1) = JK.fromText (fieldName f) J..= idx
    cell (f,i) = ingestFieldBS f (d V.! i)

sortFilterTable :: (Key -> Bool) -> [Row a] -> [Row a]
sortFilterTable f = sortOn rowKey . filter (f . rowKey)

readTable :: FilePath -> Field -> (Int, Int) -> T.Text -> Fields -> IO Table
readTable path keyf range name fields =
  liftIO (doesPathExist file) >>= \ex ->
  if not ex then do
    hPutStrLn stderr $ "Treating missing file as empty: " ++ file
    return $ Table (TableInfo name (-1) V.empty) []
  else do
  (hd, rows) <- loadECSV file $ V.filter ((Just "_index" /=) . fieldIngest) fields
  let geti "_index" = return $ -1
      geti n = maybe (fail $ file ++ " missing field " ++ BSC.unpack n) return $ V.elemIndex n hd
  key <- geti (fieldsrc keyf)
  cols <- mapM (geti . fieldsrc) fields
  let ti = TableInfo
        { tableName = name
        , tableKeyIndex = key
        , tableFields = V.zip fields cols
        }
  return
    $ (if dups then parseTableDups else parseTable) ti
    $ (if singlesort then sortFilterTable (keyInRange range) else id)
    $ parseRows ti rows
  where
  singlesort = T.isPrefixOf "Nss" name
  dups = name == "NssTwoBodyOrbit"
  name' = T.unpack name
  file 
    | singlesort = joinFile' path name' "1"
    | otherwise = joinFile path name' range
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
joinFile' :: FilePath -> String -> String -> FilePath
joinFile' bd dn sf = bd FP.</> dn FP.</> (dn ++ "_" ++ sf) FP.<.> "csv"

-- |Inverse of 'splitFile'
joinFile :: FilePath -> String -> (Int, Int) -> FilePath
joinFile bd dn (i, j) = joinFile' bd dn (r i ++ "-" ++ r j) where
  r = printf "%06d"

nextKey :: Key -> J.Series -> Table -> (J.Series, Table)
nextKey k b ts@Table{ tableRows = Row{ rowKey = tk, rowData = r }:l } = case compare k tk of
  LT -> (b, ts)
  EQ -> (b <> r, ts{ tableRows = l })
  GT -> error $ T.unpack (tableName (tableInfo ts)) ++ " out of order at " ++ show tk ++ " > " ++ show k
nextKey _ b ts@Table{ tableRows = [] } = (b, ts)

nextKeys :: Key -> J.Series -> [Table] -> (J.Series, [Table])
nextKeys = mapAccumL . nextKey

zipTables :: Table -> [Table] -> [(Key, J.Series)]
zipTables m@Table{ tableRows = Row{ rowKey = k, rowData = r }:l } tl =
  (k, r') : zipTables m{ tableRows = l } tl'
  where (r', tl') = nextKeys k r tl
zipTables Table{ tableRows = [] } tl =
  concatMap (\t -> (\Row{ rowKey = k } -> error $ "leftover in " ++ T.unpack (tableName (tableInfo t)) ++ ": " ++ show k) <$> tableRows t) tl

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n l = p : groupsOf n r where
  (p, r) = splitAt n l

ingestGaiaDR3 :: Ingest -> M Word64
ingestGaiaDR3 ing@Ingest{ ingestCatalog = cat } = do
  (keytab, keyf) <- split <$> lookupField cat True keyn
  unless (T.unpack keytab == maintabn) $ fail "file path doesn't match key table"
  tabmap <- liftIO $ HM.traverseWithKey (readTable basedir keyf range) tabfs
  let maintab = tabmap HM.! keytab
      tabs = HM.elems $ HM.delete keytab tabmap
      doc (k, b) = (show k, ingestJConsts ing <> fileUpper J..= snd range <> b)
      run o l = do
        liftIO $ putStr (show o ++ "\r") >> hFlush stdout
        ES.createBulk cat l
        return $ o + fromIntegral (length l)
  foldlM run (ingestOffset ing)
    $ groupsOf (fromIntegral $ ingestBlockSize ing) $ map doc
    $ genericDrop (ingestOffset ing) $ zipTables maintab tabs
  where
  (basedir, maintabn, range) = splitFile (ingestFile ing)
  Just keyn = catalogKey cat
  tabfs = HM.map V.fromList $ V.foldr addf HM.empty $ catalogFields cat
  addf f
    | fieldName f == fileUpper = id
    | otherwise = uncurry (HM.insertWith (++)) $ second return $ split f
  split f@Field{ fieldIngest = Just i } =
    second (\n -> f{ fieldIngest = snd <$> T.uncons n })
      $ T.breakOn "." i
  split f = error $ "field missing ingest: " ++ T.unpack (fieldName f)
  fileUpper :: IsString s => s
  fileUpper = "_file_upper"
