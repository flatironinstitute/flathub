-- Export gaia ecsv to hdf5 files, uses gaiadr3fix.yml
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Bindings.HDF5 as H5
import qualified Bindings.HDF5.Error as H5E
import qualified Bindings.HDF5.Raw as H5R
import           Control.Arrow (first)
import           Control.Exception (bracket)
import           Control.Monad ((<=<), when, unless, mfilter)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Csv.Streaming as CSV
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Yaml as Y
import           Foreign.C.String (CString)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Marshal.Array (copyArray, advancePtr)
import           Foreign.Marshal.Utils (copyBytes, fillBytes)
import           Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import           Foreign.Storable (Storable(..))
import           System.Environment (getProgName, getArgs)
import qualified System.FilePath as FP
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)
import           Unsafe.Coerce (unsafeCoerce)

import Data.ECSV

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

type EnumType = Int8

data ColumnHandler
  = ColumnOmit
  | ColumnScalar
    { columnType :: !ECSVDataType
    }
  | ColumnEnum
    { columnEnum :: HM.HashMap BS.ByteString EnumType
    }
  | ColumnArray
    { columnType :: !ECSVDataType
    , columnLength :: Maybe Int
    }
  | ColumnString
    { columnLength :: Maybe Int
    }

data Column = Column
  { columnECSV :: !ECSVColumn
  , columnHandler :: !ColumnHandler
  }

columnName :: Column -> T.Text
columnName = ecsvColName . columnECSV

columns :: HM.HashMap T.Text (Maybe ColumnFixup) -> ECSVHeader -> V.Vector Column
columns fixup = V.map col . ecsvDatatype where
  col c = Column c $ maybe ColumnOmit (colh c) $ HM.findWithDefault (Just emptyFixup) (ecsvColName c) fixup
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Just (ECSVSubTypeArray ct [Nothing]) } (ColumnFixup ft l Nothing) | ct /= ECSVString =
    ColumnArray (fromMaybe ct ft) l
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Nothing } (ColumnFixup (Just t) Nothing Nothing) =
    ColumnScalar t
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Nothing } (ColumnFixup Nothing l Nothing) =
    ColumnString l
  colh ECSVColumn{ ecsvColDataType = ECSVString, ecsvColSubtype = Nothing } (ColumnFixup Nothing Nothing (Just e))
    | V.length e >= fromIntegral (maxBound :: EnumType) = error "enum too long"
    | otherwise = ColumnEnum $ V.ifoldl' (\m i v -> HM.insert (TE.encodeUtf8 v) (fromIntegral i :: EnumType) m) HM.empty e
  colh ECSVColumn{ ecsvColDataType = t, ecsvColSubtype = Nothing } (ColumnFixup Nothing Nothing Nothing) | t /= ECSVString =
    ColumnScalar t
  colh c _ = error $ "column needs fixup: " ++ T.unpack (ecsvColName c)

csvToList :: CSV.Records a -> [a]
csvToList (CSV.Cons h r) = either error id h : csvToList r
csvToList (CSV.Nil e _) = maybe [] error e

withType :: IO H5.Datatype -> (H5.Datatype -> IO a) -> IO a
withType c = bracket c H5.closeTypeID

data ValueOps a = ValueOps
  { fillValue :: a
  , withValuetype :: forall b . (H5.Datatype -> IO b) -> IO b
  , parseValue :: BS.ByteString -> Maybe a
  , valueSize :: Int
  , pokeValue :: forall b . Ptr a -> a -> IO b -> IO b
  }

data AnyValueOps = forall a. AnyValueOps (ValueOps a)

nativeValueOps :: H5.NativeType a => a -> (BS.ByteString -> Maybe a) -> ValueOps a
nativeValueOps x r = ValueOps
  { fillValue = x
  , withValuetype = ($ H5.nativeTypeOf x)
  , parseValue = r
  , valueSize = sizeOf x
  , pokeValue = \p -> (>>) . poke p
  }

defaultValueOps :: (H5.NativeType a, Read a) => a -> ValueOps a
defaultValueOps x = nativeValueOps x
  (readMaybe . BSC.unpack)

integralValueOps :: (H5.NativeType a, Read a, Integral a, Eq a, Bounded a) => ValueOps a
integralValueOps = (defaultValueOps fill)
  { parseValue = fmap fst . mfilter (BS.null . snd) . if inint
    then fmap (first fromIntegral) . BSC.readInt
    else fmap (first fromInteger ) . BSC.readInteger
  }
  where
  fill = if minb == 0 then maxb else minb
  minb = minBound
  maxb = maxBound
  inint = toInteger minb >= toInteger (minBound :: Int) && toInteger maxb <= toInteger (maxBound :: Int)

basetypeOps :: ECSVDataType -> (forall a . H5.NativeType a => ValueOps a -> b) -> b
basetypeOps ECSVBool = ($ nativeValueOps (H5R.HBool_t maxBound) bool)
  where
  bool "0" = Just $ H5R.HBool_t 0
  bool "false" = Just $ H5R.HBool_t 0
  bool "False" = Just $ H5R.HBool_t 0
  bool "1" = Just $ H5R.HBool_t 1
  bool "true" = Just $ H5R.HBool_t 1
  bool "True" = Just $ H5R.HBool_t 1
  bool _ = Nothing
basetypeOps ECSVInt8    = ($ (integralValueOps :: ValueOps Int8))
basetypeOps ECSVInt16   = ($ (integralValueOps :: ValueOps Int16))
basetypeOps ECSVInt32   = ($ (integralValueOps :: ValueOps Int32))
basetypeOps ECSVInt64   = ($ (integralValueOps :: ValueOps Int64))
basetypeOps ECSVUInt8   = ($ (integralValueOps :: ValueOps Word8))
basetypeOps ECSVUInt16  = ($ (integralValueOps :: ValueOps Word16))
basetypeOps ECSVUInt32  = ($ (integralValueOps :: ValueOps Word32))
basetypeOps ECSVUInt64  = ($ (integralValueOps :: ValueOps Word64))
basetypeOps ECSVFloat32 = ($ (defaultValueOps (unsafeCoerce (0x7fc0ffff::Word32) :: Float)))
basetypeOps ECSVFloat64 = ($ (defaultValueOps (unsafeCoerce (0x7ff80000ffffffff::Word64) :: Double)))
basetypeOps t           = ($ (error ("basetypeOps: " ++ show t) :: ValueOps H5R.HErr_t))

handlerOps :: ColumnHandler -> AnyValueOps
handlerOps ColumnOmit = error "handlerOps ColumnOmit"
handlerOps (ColumnScalar t) = basetypeOps t AnyValueOps

handlerOps (ColumnEnum e) = AnyValueOps integralValueOps
  { withValuetype = withType (H5.createEnum $ sortOn snd $ HM.toList e)
  , parseValue = (`HM.lookup` e)
  }

handlerOps (ColumnString Nothing) = AnyValueOps ValueOps
  { fillValue = BS.empty
  , withValuetype = \f -> withType (H5.createTypeID H5.String H5R.h5t_VARIABLE) $ \t -> do
      H5.setStringPad t H5.NullPad 
      H5.setCSet t H5.UTF8
      f t
  , parseValue = Just
  , valueSize = sizeOf (nullPtr :: CString)
  , pokeValue = \p s f -> BS.useAsCString s $ \sp ->
      poke (castPtr p :: Ptr CString) sp >> f
  }

handlerOps (ColumnString (Just l)) = AnyValueOps ValueOps
  { fillValue = BS.replicate l 0
  , withValuetype = \f -> withType (H5.createTypeID H5.String $ fromIntegral l) $ \t -> do
      H5.setStringPad t H5.NullPad 
      H5.setCSet t H5.UTF8
      f t
  , parseValue = Just
  , valueSize = l
  , pokeValue = \p s f -> do
      let sl = BS.length s
          p' = castPtr p :: CString
      when (sl > l) $ fail $ "string too long: " ++ show s
      BSU.unsafeUseAsCString s $ \sp -> copyBytes p' sp sl
      fillBytes (plusPtr p' sl) 0 (l - sl)
      f
  }

handlerOps (ColumnArray t Nothing) = basetypeOps t $ \baseops ->
  AnyValueOps ValueOps
    { fillValue = VS.empty
    , withValuetype = \f -> withValuetype baseops $ \bt ->
        withType (H5.createVLenType bt) f
    , parseValue = fmap VS.fromList . parseArrayWith (parseValue baseops)
    , valueSize = sizeOf (H5R.HVl_t 0 nullPtr)
    , pokeValue = \p v f -> VS.unsafeWith v $ \vp ->
        poke (castPtr p) (H5R.HVl_t (fromIntegral $ VS.length v * sizeOf (VS.head v)) (castPtr vp)) >> f
    }

handlerOps (ColumnArray t (Just l)) = basetypeOps t $ \baseops ->
  let fillv = VS.replicate l $ fillValue baseops in
  AnyValueOps ValueOps
    { fillValue = fillv
    , withValuetype = \f -> withValuetype baseops $ \bt ->
        withType (H5.createArrayType bt [fromIntegral l]) f
    , parseValue = fmap VS.fromList . parseArrayWith (parseValue baseops)
    , valueSize = l * valueSize baseops
    , pokeValue = \p v f -> do
        let vl = VS.length v
            p' = castPtr p
        when (vl > l) $ fail $ "array too long: " ++ show vl
        VS.unsafeWith v $ \vp -> copyArray p' vp vl
        VS.unsafeWith fillv $ \fp -> copyArray (advancePtr p' vl) (advancePtr fp vl) (l - vl)
        f
    }

parseArrayWith :: (BS.ByteString -> Maybe a) -> BS.ByteString -> Maybe [a]
parseArrayWith pv = mapM pv . BSC.split ',' <=< BS.stripPrefix "[" <=< BS.stripSuffix "]"

    
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

makeDataset :: H5.File -> H5.Dataspace -> Column -> V.Vector BS.ByteString -> IO ()
makeDataset _ _ Column{ columnHandler = ColumnOmit } _ = return ()
makeDataset h5f h5s col vals = case handlerOps (columnHandler col) of
  AnyValueOps ValueOps{..} ->
    withValuetype $ \h5t -> do
      dcpl <- H5.createPropertyList
      -- H5.setFillTime dcpl H5.Never
      allocaBytes valueSize $ \fp ->
        pokeValue fp fillValue $
          H5E.withErrorCheck_ $ H5R.h5p_set_fill_value (H5.hid dcpl) (H5.hid h5t) (H5R.In fp)
      bracket
        (H5.createDataset h5f (TE.encodeUtf8 $ columnName col) h5t h5s Nothing (Just dcpl) Nothing)
        H5.closeDataset $ \h5d -> do
          mapM_ (createStringAttribute h5d "description") $ ecsvColDescription $ columnECSV col
          mapM_ (createStringAttribute h5d "unit") $ ecsvColUnit $ columnECSV col
          allocaBytes (valueSize * V.length vals) $ \vp -> do
            V.ifoldr (\i s -> pokeValue (plusPtr vp $ valueSize * i)
                (if BS.null s then fillValue else
                  fromMaybe (error $ "parseValue " ++ T.unpack (columnName col) ++ " " ++ show i ++ ": " ++ BSC.unpack s) $ parseValue s))
              (H5E.withErrorCheck_ $ H5R.h5d_write (H5.hid h5d) (H5.hid h5t) H5R.h5s_ALL H5R.h5s_ALL H5R.h5p_DEFAULT (H5R.InArray vp))
              vals

convert :: Maybe FilePath -> FilePath -> IO ()
convert outdir file = do
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
      len = V.length csvv
  bracket
    (H5.createFile (BSC.pack outfile) [H5.Truncate] Nothing Nothing)
    H5.closeFile $ \h5f -> bracket
      (H5.createSimpleDataspace [fromIntegral len])
      H5.closeDataspace $ \h5s -> 
        V.imapM_ (\i col -> makeDataset h5f h5s col (V.map (V.! i) csvv)) cols
  where
  Just basename = FP.stripExtension "csv" (FP.takeFileName file)
  filedir = FP.takeDirectory file
  tabname = FP.takeFileName filedir
  basedir = FP.takeDirectory $ FP.takeDirectory filedir
  outfile = fromMaybe (basedir FP.</> "hdf5" FP.</> tabname) outdir FP.</> basename FP.<.> "hdf5"

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  (outdir, files) <- case args of
    [] -> (Nothing, []) <$ hPutStrLn stderr ("Usage: " ++ prog ++ " [OUTDIR] FILE.csv ...")
    outdir:files
      | FP.isExtensionOf "csv" outdir -> return (Nothing, args)
      | otherwise -> return (Just outdir, files)
  mapM_ (convert outdir) files
