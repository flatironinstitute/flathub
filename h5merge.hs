{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import qualified Bindings.HDF5 as H5
import           Bindings.HDF5.Error (HDF5Exception)
import qualified Codec.Compression.GZip as GZ
import           Control.Concurrent (forkFinally)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.QSemN
import           Control.Exception (catch, throwIO)
import           Control.Monad ((<=<), foldM, when, unless, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Csv as CSV
import           Data.Foldable (fold)
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int8, Int64)
import           Data.List (isPrefixOf, sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VSort
import qualified Data.Vector.Storable as VS
import           Data.Word (Word32, Word64)
import qualified Data.Yaml as YAML
import           System.Directory (listDirectory)
import           System.Environment (getArgs)
import           System.FilePath ((</>), takeExtensions)
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

compareStrNum :: String -> String -> Ordering
compareStrNum a@(a0:ar) b@(b0:br) =
  fold compareRead <> compare a0 b0 <> compareStrNum ar br <> compare a b
  where
  compareRead = do
    (x, _) <- reads a
    (y, _) <- reads b
    return $ compare x (y :: Word)
compareStrNum a b = compare a b

compareBSNum :: BS.ByteString -> BS.ByteString -> Ordering
compareBSNum a b = compare (BS.length a) (BS.length b) <> compare a b

main :: IO ()
main = do
  (catname:indir:outfile:args) <- getArgs
  let (start, stop) = case args of
        [] -> (0, maxBound)
        [x] -> (read x, maxBound)
        [x, y] -> (read x, read y)
        _ -> error "Unhandled args"
  catalog <- either throwIO (return . (HM.! catname)) =<< YAML.decodeFileEither "catalogs.yml"
  let fields = V.fromList $ expandFields $ catalogFields catalog
      fieldNames = V.map fieldNameBS fields
      Just solutionIdx = V.elemIndex "solution_id" fieldNames
      Just sortIdx = V.elemIndex "source_id" fieldNames
      fields' = V.filter (fieldDisp . snd) $ V.indexed fields

  infiles <- sortBy compareStrNum . filter (isPrefixOf ".csv" . takeExtensions) <$> listDirectory indir

  R.runResourceT $ do
    let allocate o c = snd <$> R.allocate o c

    hf <- H5.openFile (BSC.pack outfile) [H5.Create, H5.ReadWrite] Nothing
      `allocate` (print <=< H5.closeFile)

    hs <- H5.createSimpleDataspace [fromIntegral totalCount]
      `allocate` H5.closeDataspace

    hds' <- mapM (\(_, f) ->
      (H5.createDataset hf (fieldNameBS f) (h5Type $ fieldType f) hs Nothing Nothing Nothing
        `catch` \(_ :: HDF5Exception) -> H5.openDataset hf (fieldNameBS f) Nothing)
        `allocate` H5.closeDataset) fields'

    par <- newQSemN 8 `allocate` (`waitQSemN` 8)
    n <- foldM (\off infile -> if off >= stop then return off else do
      csv <- liftIO $ either fail return . CSV.decode CSV.NoHeader
        . (case takeExtensions infile of
          ".csv.gz" -> GZ.decompress
          ".csv" -> id
          _ -> error "Unspported CSV file")
        =<< BSLC.readFile (indir </> infile)
      let header = V.head csv
          urows = V.tail csv
          len = fromIntegral (V.length urows)
          off' = off + len
      unless (header == fieldNames) $ fail $ "Mismatching fields : " ++ show header
      liftIO $ waitQSemN par 1
      liftIO $ putStrLn $ infile ++ ": " ++ show off ++ "-" ++ show off'
      when (off' > start) $ void $ R.resourceForkWith (`forkFinally` \_ -> signalQSemN par 1) $ do
        mrows <- V.unsafeThaw urows
        VSort.sortBy (compareBSNum `on` (V.! sortIdx)) mrows
        rows <- V.unsafeFreeze mrows
        let col i = parseFieldValues (fields V.! i) (V.! i) rows
        case col solutionIdx of
          Long x | VS.all (solutionID ==) x -> return ()
          _ -> fail "Solution_id mismatch"
        (hspr, hsp) <- R.allocate (H5.copyDataspace hs) H5.closeDataspace
        (memr, mem) <- R.allocate (H5.createSimpleDataspace [len]) H5.closeDataspace
        liftIO $ H5.selectHyperslab hsp H5.Set [(off, Nothing, len, Nothing)]
        wv <- V.imapM (\i' (i, _) -> do
            wv <- liftIO $ newEmptyMVar
            void $ R.resourceForkWith (`forkFinally` putMVar wv) $
              liftIO $ hdf5WriteType (hds' V.! i') hsp (col i) mem
            return wv)
          fields'
        liftIO $ V.mapM_ takeMVar wv
        R.release memr
        R.release hspr
      return off')
      0 infiles
    liftIO $ unless (n > stop || n == totalCount) $ putStrLn "Incorrect totalCount"
