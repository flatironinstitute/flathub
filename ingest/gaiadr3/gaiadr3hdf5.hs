-- Export gaia ecsv to hdf5 files, uses gaiadr3fix.yml
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Bindings.HDF5 as H5
import qualified Bindings.HDF5.Error as H5E
import qualified Bindings.HDF5.Raw as H5R
import           Control.Arrow (first)
import           Control.Exception (bracket)
import           Control.Monad (when, unless, mfilter)
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
import           Data.Proxy (Proxy(Proxy))
import           Data.Tagged (Tagged(Tagged))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Yaml as Y
import           Foreign.C.String (CString)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Marshal.Utils (copyBytes, fillBytes)
import           Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import           Foreign.Storable (Storable(..))
import           GHC.TypeLits (Nat, KnownNat, natVal)
import           System.Environment (getProgName, getArgs)
import qualified System.FilePath as FP
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)
import           Unsafe.Coerce (unsafeCoerce)

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

type EnumType = Int8
enumType :: ECSVDataType
enumType = ECSVInt8

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
  { fillValue :: Maybe a
  , withValuetype :: forall b . (H5.Datatype -> IO b) -> IO b
  , parseValue :: BS.ByteString -> Maybe a
  , valueSize :: Int
  , pokeValue :: forall b . Ptr a -> a -> IO b -> IO b
  }

data AnyValueOps = forall a. AnyValueOps (ValueOps a)

nativeValueOps :: H5.NativeType a => a -> (BS.ByteString -> Maybe a) -> ValueOps a
nativeValueOps x r = ValueOps
  { fillValue = Just x
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

basetypeOps :: ECSVDataType -> AnyValueOps
basetypeOps ECSVBool = AnyValueOps $ nativeValueOps (H5R.HBool_t maxBound) bool
  where
  bool "0" = Just $ H5R.HBool_t 0
  bool "false" = Just $ H5R.HBool_t 0
  bool "False" = Just $ H5R.HBool_t 0
  bool "1" = Just $ H5R.HBool_t 1
  bool "true" = Just $ H5R.HBool_t 1
  bool "True" = Just $ H5R.HBool_t 1
  bool _ = Nothing
basetypeOps ECSVInt8    = AnyValueOps (integralValueOps :: ValueOps Int8)
basetypeOps ECSVInt16   = AnyValueOps (integralValueOps :: ValueOps Int16)
basetypeOps ECSVInt32   = AnyValueOps (integralValueOps :: ValueOps Int32)
basetypeOps ECSVInt64   = AnyValueOps (integralValueOps :: ValueOps Int64)
basetypeOps ECSVUInt8   = AnyValueOps (integralValueOps :: ValueOps Word8)
basetypeOps ECSVUInt16  = AnyValueOps (integralValueOps :: ValueOps Word16)
basetypeOps ECSVUInt32  = AnyValueOps (integralValueOps :: ValueOps Word32)
basetypeOps ECSVUInt64  = AnyValueOps (integralValueOps :: ValueOps Word64)
basetypeOps ECSVFloat32 = AnyValueOps $ (defaultValueOps (unsafeCoerce (0x7fc0ffff::Word32) :: Float))
basetypeOps ECSVFloat64 = AnyValueOps $ (defaultValueOps (unsafeCoerce (0x7ff80000ffffffff::Word64) :: Double))
basetypeOps t           = error $ "basetypeOps: " ++ show t

handlerOps :: ColumnHandler -> AnyValueOps
handlerOps ColumnOmit = error "handlerOps ColumnOmit"
handlerOps (ColumnScalar t) = basetypeOps t

handlerOps (ColumnEnum e) = AnyValueOps integralValueOps
  { withValuetype = withType (H5.createEnum $ sortOn snd $ HM.toList e)
  , parseValue = (`HM.lookup` e)
  }

handlerOps (ColumnString Nothing) = AnyValueOps ValueOps
  { fillValue = Nothing
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
  { fillValue = Just $ BS.replicate l 0
  , withValuetype = \f -> withType (H5.createTypeID H5.String $ fromIntegral l) $ \t -> do
      H5.setStringPad t H5.NullPad 
      H5.setCSet t H5.UTF8
      f t
  , parseValue = Just
  , valueSize = l
  , pokeValue = \p s f -> BSU.unsafeUseAsCStringLen s $ \(sp, sl) -> do
      when (sl > l) $ fail $ "string too long: " ++ show s
      let p' = castPtr p :: CString
      copyBytes p' sp sl
      fillBytes (plusPtr p' sl) 0 (l - sl)
      f
  }


{-
withDatatype :: ColumnHandler -> (forall a . Maybe (H5R.In a) -> H5.Datatype -> IO ()) -> IO ()
withDatatype ColumnOmit _ = return ()
withDatatype (ColumnScalar t) f = withBasetype t $ \x -> 
  H5R.withIn x $ \i -> f (Just i) (H5.nativeTypeOf x)
withDatatype (ColumnEnum e) f = withBasetype enumType $ \x ->
  H5R.withIn x $
    withType (H5.createEnum $ sortOn snd $ HM.toList e) . f . Just
withDatatype (ColumnArray t (Just l)) f = withBasetype t $ \x ->
  H5R.withInVector (VS.replicate l x) $
    withType (H5.createArrayType (H5.nativeTypeOf x) [fromIntegral l]) . f . Just . H5R.In . H5R.unwrapPtr
withDatatype (ColumnArray t Nothing) f = withBasetype t $ \x ->
  withType (H5.createVLenType (H5.nativeTypeOf x)) $ f Nothing
withDatatype (ColumnString (Just l)) f =
  H5R.withInByteString (BS.replicate l 0) $ \(H5R.InArray i) ->
    withType (H5.createTypeID H5.String (fromIntegral l)) $ \t -> do
      H5.setStringPad t H5.NullPad 
      H5.setCSet t H5.UTF8
      f (Just $ H5R.In i) t
withDatatype (ColumnString Nothing) f =
  withType (H5.createTypeID H5.String H5R.h5t_VARIABLE) $ \t -> do
    H5.setStringPad t H5.NullPad 
    H5.setCSet t H5.UTF8
    f Nothing t

newtype FixedArray a (l :: Nat) = FixedArray (VS.Vector a)
newtype FixedString (l :: Nat) = FixedString BS.ByteString
newtype VarArray a = VarArray (VS.Vector a)
newtype VarString = VarString BS.ByteString

fixedLength :: KnownNat l => p l -> Int
fixedLength = fromInteger . natVal

instance (KnownNat l, Storable a) => Storable (FixedArray a l) where
  sizeOf a@(FixedArray v) = fixedLength a * sizeOf (VS.head v)
  alignment (FixedArray v) = alignment (VS.head v)
  peek p = FixedArray <$> VS.generateM (fixedLength (Proxy :: Proxy l)) (peekElemOff $ castPtr p)
  poke p (FixedArray v) = VS.imapM_ (pokeElemOff $ castPtr p) v

instance KnownNat l => Storable (FixedString l) where
  sizeOf (FixedString s) = fromInteger (natVal (Proxy :: Proxy l)) * sizeOf (BS.head s)
  alignment (FixedString s) = alignment (BS.head s)
  peek p = FixedString <$> BS.packCStringLen (castPtr p, fixedLength (Proxy :: Proxy l))
  poke p (FixedString s) = BSU.unsafeUseAsCStringLen s $ \(sp, sl) -> fail "TODO"
    -}
    
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
      mapM_ (\x -> do
        H5.setFillTime dcpl H5.Never
        allocaBytes valueSize $ \xp ->
          pokeValue xp x $
            H5E.withErrorCheck_ $ H5R.h5p_set_fill_value (H5.hid dcpl) (H5.hid h5t) (H5R.In xp))
        fillValue
      bracket
        (H5.createDataset h5f (TE.encodeUtf8 $ columnName col) h5t h5s Nothing (Just dcpl) Nothing)
        H5.closeDataset $ \h5d -> do
          mapM_ (createStringAttribute h5d "description") $ ecsvColDescription $ columnECSV col
          mapM_ (createStringAttribute h5d "unit") $ ecsvColUnit $ columnECSV col
          allocaBytes (valueSize * V.length vals) $ \vp -> do
            H5E.withErrorCheck_ $ H5R.h5d_write (H5.hid h5d) (H5.hid h5t) H5R.h5s_ALL H5R.h5s_ALL H5R.h5p_DEFAULT (H5R.InArray vp)

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
