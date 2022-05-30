-- Export gaia ecsv to hdf5 files, uses gaiadr3fix.yml
--
{-# LANGUAGE OverloadedStrings #-}

import qualified Bindings.HDF5 as H5
import qualified Bindings.HDF5.Error as H5E
import qualified Bindings.HDF5.Raw as H5R
import           Control.Arrow (first)
import           Control.Exception (bracket)
import           Control.Monad (unless)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Csv.Streaming as CSV
import           Data.Foldable (find, foldl')
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Yaml as Y
import           System.Environment (getArgs)
import qualified System.FilePath as FP

import Data.ECSV

import Debug.Trace

data ColumnFixup = ColumnFixup
  { fixupType :: Maybe ECSVDataType
  , fixupLength :: Maybe Int -- ^array or string
  , fixupEnum :: Maybe (V.Vector T.Text)
  }

emptyFixup :: ColumnFixup
emptyFixup = ColumnFixup Nothing Nothing Nothing

instance J.FromJSON ColumnFixup where
  parseJSON = J.withObject "fixup" $ \o -> ColumnFixup
    <$> o J..:? "type"
    <*> o J..:? "length"
    <*> o J..:? "enum"

data ColumnHandler
  = ColumnOmit
  | ColumnScalar
    { columnType :: !ECSVDataType
    }
  | ColumnEnum
    { columnType :: !ECSVDataType
    , columnEnum :: HM.HashMap BS.ByteString Int
    }
  | ColumnArray
    { columnType :: !ECSVDataType
    , columnLength :: Maybe Int
    }
  | ColumnString
    { columnLength :: Maybe Int
    }
  deriving (Show)

data Column = Column
  { columnECSV :: !ECSVColumn
  , columnHandler :: !ColumnHandler
  } deriving (Show)

columnName :: Column -> T.Text
columnName = ecsvColName . columnECSV

enumType :: Int -> ECSVDataType
enumType x
  | x < 127 = ECSVInt8
  | otherwise = error "enum too long"

columns :: HM.HashMap T.Text (Maybe ColumnFixup) -> ECSVHeader -> V.Vector Column
columns fixup = V.map col . ecsvDatatype where
  col c = Column c $ maybe ColumnOmit (colh c) $ HM.findWithDefault (Just emptyFixup) (ecsvColName c) fixup
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Just (ECSVSubTypeArray ct [Nothing]) } (ColumnFixup ft l Nothing) | ct /= ECSVString = ColumnArray (fromMaybe ct ft) l
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Nothing } (ColumnFixup (Just t) Nothing Nothing) = ColumnScalar t
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Nothing } (ColumnFixup Nothing l Nothing) = ColumnString l
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Nothing } (ColumnFixup t Nothing (Just e)) = ColumnEnum (fromMaybe (enumType $ V.length e) t) $ V.ifoldl' (\m i v -> HM.insert (TE.encodeUtf8 v) i m) HM.empty e
  colh ECSVColumn{ ecsvColDataType = t, ecsvColSubtype = Nothing } (ColumnFixup Nothing Nothing Nothing) | t /= ECSVString = ColumnScalar t
  colh c _ = error $ "column needs fixup: " ++ T.unpack (ecsvColName c)

csvToList :: CSV.Records a -> [a]
csvToList (CSV.Cons h r) = either error id h : csvToList r
csvToList (CSV.Nil e _) = maybe [] error e

baseDatatype :: ECSVDataType -> H5.Datatype
baseDatatype ECSVBool    = H5.nativeTypeOf (H5R.HBool_t 0)
baseDatatype ECSVInt8    = H5.nativeTypeOf (0 :: Int8)
baseDatatype ECSVInt16   = H5.nativeTypeOf (0 :: Int16)
baseDatatype ECSVInt32   = H5.nativeTypeOf (0 :: Int32)
baseDatatype ECSVInt64   = H5.nativeTypeOf (0 :: Int64)
baseDatatype ECSVUInt8   = H5.nativeTypeOf (0 :: Word8)
baseDatatype ECSVUInt16  = H5.nativeTypeOf (0 :: Word16)
baseDatatype ECSVUInt32  = H5.nativeTypeOf (0 :: Word32)
baseDatatype ECSVUInt64  = H5.nativeTypeOf (0 :: Word64)
baseDatatype ECSVFloat32 = H5.nativeTypeOf (0 :: Float)
baseDatatype ECSVFloat64 = H5.nativeTypeOf (0 :: Double)
baseDatatype t           = error $ "baseDatatype " ++ show t

withType :: IO H5.Datatype -> (H5.Datatype -> IO a) -> IO a
withType c = bracket c H5.closeTypeID

withDatatype :: ColumnHandler -> (H5.Datatype -> IO ()) -> IO ()
withDatatype ColumnOmit = const $ return ()
withDatatype (ColumnScalar t) = ($ baseDatatype t)
withDatatype (ColumnEnum t e) =
  withType (H5.createEnum $ HM.toList e)
withDatatype (ColumnArray t (Just l)) =
  withType (H5.createArrayType (baseDatatype t) [fromIntegral l])
withDatatype (ColumnArray t Nothing) =
  withType (H5.createVLenType (baseDatatype t))
withDatatype (ColumnString l) =
  withType (H5.createTypeID H5.String (maybe H5R.h5t_VARIABLE fromIntegral l))

withDataset :: H5.File -> H5.Dataspace -> Column -> (H5.Dataset -> IO ()) -> IO ()
withDataset f s col act =
  withDatatype (columnHandler col) $ \t -> bracket
    (H5.createDataset f (TE.encodeUtf8 $ columnName col) t s Nothing Nothing Nothing)
    H5.closeDataset act

createStringAttribute :: H5.Location loc => loc -> BS.ByteString -> T.Text -> IO ()
createStringAttribute loc name val =
  withType (H5.createTypeID H5.String (fromIntegral $ BS.length val')) $ \t -> do
    H5.setStringPad t H5.NullPad 
    H5.setCSet t H5.UTF8
    bracket
      (H5.createDataspace H5.Scalar)
      H5.closeDataspace $ \s -> bracket
        (H5.createAttribute loc name t s)
        H5.closeAttribute $ \a ->
          H5R.withInByteString val' $
            H5E.withErrorCheck_ .
              H5R.h5a_write (H5.hid a) (H5.hid t)
  where
  val' = TE.encodeUtf8 val

convert :: FilePath -> IO ()
convert file = do
  putStrLn file
  fixup <- HM.findWithDefault mempty tabname <$> Y.decodeFileThrow "gaiadr3fix.yml"
  (ecsv, dat) <- first (either (error . show) id) . parseECSVHeader <$> BSLC.readFile file
  let CSV.Cons csvh' csv = CSV.decode CSV.NoHeader dat
  csvh <- either fail return csvh'
  let cols = columns fixup ecsv
  unless (csvh == V.map (TE.encodeUtf8 . ecsvColName) (ecsvDatatype ecsv)) $
    fail "ECSV/CSV header mismatch"
  -- might as well read it all in now for efficiency
  let csvv = V.fromList $ csvToList csv
      len = 10 -- V.length csvv
  bracket
    (H5.createFile (BSC.pack outfile) [H5.Truncate] Nothing Nothing)
    H5.closeFile $ \h5f -> bracket
      (H5.createSimpleDataspace [fromIntegral len])
      H5.closeDataspace $ \h5s -> 
        V.imapM_ (\i col -> withDataset h5f h5s col $ \h5d -> do
          mapM_ (createStringAttribute h5d "description") $ ecsvColDescription $ columnECSV col
          mapM_ (createStringAttribute h5d "unit") $ ecsvColUnit $ columnECSV col
          return ()) cols
  where
  Just basename = FP.stripExtension "csv" (FP.takeFileName file)
  filedir = FP.takeDirectory file
  tabname = FP.takeFileName filedir
  basedir = FP.takeDirectory $ FP.takeDirectory filedir
  outfile = {- basedir FP.</> "hdf5" FP.</> tabname FP.</> -} basename FP.<.> "hdf5"

main :: IO ()
main = mapM_ convert =<< getArgs
