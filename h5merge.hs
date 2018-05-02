{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import qualified Bindings.HDF5 as H5
import           Bindings.HDF5.Error (HDF5Exception)
import qualified Codec.Compression.GZip as GZ
import           Control.Concurrent (forkFinally)
import           Control.Concurrent.QSemN
import           Control.Exception (bracket, catch, throwIO)
import           Control.Monad ((<=<), foldM, unless, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Csv as CSV
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int8, Int64)
import           Data.List (stripPrefix, sort)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VSort
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word (Word32, Word64)
import qualified Data.Yaml as YAML
import           System.Directory (listDirectory)
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           Text.Read (readMaybe)
import           Unsafe.Coerce (unsafeCoerce)

import Schema

totalCount :: H5.HSize
totalCount = 1692919135

solutionID :: Int64
solutionID = 1635721458409799680

h5Type :: Type -> H5.Datatype
h5Type (Double x) = H5.nativeTypeOf1 x
h5Type (Float x) = H5.nativeTypeOf1 x
h5Type (Long x) = H5.nativeTypeOf1 x
h5Type (Integer x) = H5.nativeTypeOf1 x
h5Type (Short x) = H5.nativeTypeOf1 x
h5Type (Byte x) = H5.nativeTypeOf1 x
h5Type (Boolean _) = H5.nativeTypeOf (0 :: Int8)
h5Type t = error $ "Unsupported type: " ++ show t

fieldNameBS :: Field -> BS.ByteString
fieldNameBS = TE.encodeUtf8 . fieldName

parseVal :: Read a => Type -> a -> BS.ByteString -> a
parseVal _ x "" = x
parseVal t _ b = fromMaybe (error $ "no parse for " ++ show t ++ ": " ++ s) $ readMaybe s
  where s = BSC.unpack b

parseFieldValues :: Field -> (a -> BS.ByteString) -> V.Vector a -> TypeValue VS.Vector
parseFieldValues Field{ fieldType = t@(Double  _) } f = Double  . VS.convert . V.map (parseVal t (unsafeCoerce (0x7ff80000ffffffff::Word64)) . f)
parseFieldValues Field{ fieldType = t@(Float   _) } f = Float   . VS.convert . V.map (parseVal t (unsafeCoerce (0x7fc0ffff::Word32)) . f)
parseFieldValues Field{ fieldType = t@(Long    _) } f = Long    . VS.convert . V.map (parseVal t (-1) . f)
parseFieldValues Field{ fieldType = t@(Integer _) } f = Integer . VS.convert . V.map (parseVal t (-1) . f)
parseFieldValues Field{ fieldType = t@(Short   _) } f = Short   . VS.convert . V.map (parseVal t (-1) . f)
parseFieldValues Field{ fieldType = Byte{}, fieldName = "phot_variable_flag" } f = Byte . VS.convert . V.map (\a -> case f a of
  { "NOT_AVAILABLE" -> -1 ; "CONSTANT" -> 0 ; "VARIABLE" -> 1 ; "" -> -1
  ; s -> error $ "no parse for phot_variable_flag: " ++ BSC.unpack s })
parseFieldValues Field{ fieldType = t@(Byte    _) } f = Byte    . VS.convert . V.map (parseVal t (-1) . f)
parseFieldValues Field{ fieldType = (Boolean   _) } f = Byte    . VS.convert . V.map (\a -> case f a of
  { "false" -> 0 ; "true" -> 1 ; "" -> -1
  ; s -> error $ "no parse for boolean: " ++ BSC.unpack s })
parseFieldValues Field{ fieldType = t } _ = error $ "Unsupported type: " ++ show t

hdf5WriteVector :: (H5.NativeType a, Show a) => H5.Dataset -> H5.Dataspace -> VS.Vector a -> H5.Dataspace -> IO ()
hdf5WriteVector d s v m = H5.writeDataset d (Just m) (Just s) Nothing v

hdf5WriteType :: H5.Dataset -> H5.Dataspace -> TypeValue VS.Vector -> H5.Dataspace -> IO ()
hdf5WriteType d s (Long    v) m = hdf5WriteVector d s v m
hdf5WriteType d s (Integer v) m = hdf5WriteVector d s v m
hdf5WriteType d s (Short   v) m = hdf5WriteVector d s v m
hdf5WriteType d s (Byte    v) m = hdf5WriteVector d s v m
hdf5WriteType d s (Double  v) m = hdf5WriteVector d s v m
hdf5WriteType d s (Float   v) m = hdf5WriteVector d s v m
hdf5WriteType _ _ t           _ = fail $ "Unsupported HDF5 type: " ++ show (typeOfValue t)

hdf5Read1 :: (H5.NativeType a, Show a) => H5.Dataset -> H5.Dataspace -> H5.HSize -> IO a
hdf5Read1 d s o = do
  H5.selectElements s H5.Set (V.singleton (VS.singleton o))
  v <- VSM.new 1
  bracket (H5.createSimpleDataspace [1]) H5.closeDataspace $ \m ->
    H5.readDatasetInto d (Just m) (Just s) Nothing v
  VSM.unsafeRead v 0

hdf5BinSearch :: (H5.NativeType a, Show a) => H5.Dataset -> H5.Dataspace -> (a -> Bool) -> (H5.HSize, H5.HSize) -> IO H5.HSize
hdf5BinSearch d s f (a, b) = do
  x <- hdf5Read1 d s i
  -- putStrLn $ "testing " ++ show ab ++ " " ++ show i ++ " = " ++ show x
  (if i == a then return . snd else hdf5BinSearch d s f)
    $ if f x then (a, i) else (i, b)
  where i = (a + b) `div` 2

compareBSNum :: BS.ByteString -> BS.ByteString -> Ordering
compareBSNum a b = compare (BS.length a) (BS.length b) <> compare a b

type SourceID = Int64

data SourceFile = SourceFile
  { sourceFileMin, sourceFileMax :: !SourceID
  } deriving (Eq, Ord)

sourceFilePath :: SourceFile -> FilePath
sourceFilePath (SourceFile a b) = "GaiaSource_" ++ show a ++ '_' : show b ++ ".csv.gz"

sourceFile :: FilePath -> Maybe SourceFile
sourceFile f = do
  a <- stripPrefix "GaiaSource_" f
  (x, '_':b) <- listToMaybe $ reads a
  (y, ".csv.gz") <- listToMaybe $ reads b
  return $ SourceFile x y

main :: IO ()
main = do
  (catname:indir:outfile:args) <- getArgs
  (startoff, limit) <- case args of
    [] -> return (Nothing, maxBound)
    ["-"] -> return (Nothing, maxBound)
    [start] -> return (Just $ read start, maxBound)
    ["-",end] -> return (Nothing, read end)
    [start,end] -> return (Just $ read start, read end)
    _ -> fail "Unhandled args"
  catalog <- either throwIO (return . (HM.! catname)) =<< YAML.decodeFileEither "catalogs.yml"
  let fields = V.fromList $ catalogFields catalog
      fieldNames = V.map fieldNameBS fields
      Just sortIdx = do
        k <- catalogKey catalog
        V.findIndex ((k ==) . fieldName) fields
      Just solutionIdx = V.elemIndex "solution_id" fieldNames
      fields' = V.filter (fieldDisp . snd) $ V.indexed fields
      Just sortIdx' = V.findIndex ((sortIdx ==) . fst) fields'

  -- liftIO $ print $ fieldNameBS $ fields V.! sortIdx

  srcs <- sort . mapMaybe sourceFile <$> listDirectory indir

  R.runResourceT $ do
    let allocate o c = snd <$> R.allocate o c

    hf <- H5.openFile (BSC.pack outfile) [H5.Create, H5.ReadWrite] Nothing
      `allocate` (print <=< H5.closeFile)

    hs <- H5.createSimpleDataspace [fromIntegral totalCount]
      `allocate` H5.closeDataspace

    hds' <- V.mapM (\(_, f) ->
      (H5.createDataset hf (fieldNameBS f) (h5Type $ fieldType f) hs Nothing Nothing Nothing
        `catch` \(_ :: HDF5Exception) -> H5.openDataset hf (fieldNameBS f) Nothing)
        `allocate` H5.closeDataset) fields'

    Just off0 <- liftIO $ maybe (fmap fromIntegral . VS.findIndex ((0 :: Float) ==) <$> H5.readDataset (V.last hds') Nothing Nothing) (return . Just) startoff
    (start, srcs') <- liftIO $ if off0 == 0 then return (0, srcs) else do
      let hds = hds' V.! sortIdx'
      key0 <- hdf5Read1 hds hs (pred off0)
      let srcs' = dropWhile ((key0 >) . sourceFileMax) srcs
          start = sourceFileMin $ head srcs'
      off <- hdf5BinSearch hds hs (\x -> x >= start || x == 0) (0, off0)
      s <- hdf5Read1 hds hs off
      let desc = '[' : show off ++ "] = " ++ show s
      unless (s == start) $ fail $ "Start offset mismatch: " ++ desc ++ " /= " ++ show start
      putStrLn $ "Starting at " ++ desc
      return (off, srcs')

    par <- newQSemN 28 `allocate` (`waitQSemN` 28)
    n <- foldM (\off src -> do
      csv <- liftIO $ either fail return
        . CSV.decode CSV.NoHeader
        . GZ.decompress
        =<< BSLC.readFile (indir </> sourceFilePath src)
      let header = V.head csv
          urows = V.tail csv
          len = fromIntegral (V.length urows)
          off' = off + len
      unless (header == fieldNames) $ fail $ "Mismatching fields : " ++ show header
      liftIO $ waitQSemN par 1
      liftIO $ putStrLn $ sourceFilePath src ++ ": " ++ show off ++ "-" ++ show off'
      void $ R.resourceForkWith (`forkFinally` \_ -> signalQSemN par 1) $ liftIO $ do
        mrows <- V.unsafeThaw urows
        VSort.sortBy (compareBSNum `on` (V.! sortIdx)) mrows
        rows <- V.unsafeFreeze mrows
        let col i = parseFieldValues (fields V.! i) (V.! i) rows
        case col solutionIdx of
          Long x | VS.all (solutionID ==) x -> return ()
          _ -> fail "Solution_id mismatch"
        bracket (H5.copyDataspace hs) H5.closeDataspace $ \hsp ->
          bracket (H5.createSimpleDataspace [len]) H5.closeDataspace $ \mem -> do
            H5.selectHyperslab hsp H5.Set [(off, Nothing, len, Nothing)]
            V.imapM_ (\i' (i, _) -> 
                hdf5WriteType (hds' V.! i') hsp (col i) mem)
              fields'
      return off')
      start $ take limit srcs'
    liftIO $ unless (n == totalCount) $ putStrLn "Incorrect totalCount"
