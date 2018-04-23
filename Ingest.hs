{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest
  ( ingestHDF5
  ) where

import           Control.Arrow (first)
import           Control.Exception (bracket)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Control (liftBaseOp)
import qualified Bindings.HDF5 as H5
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word (Word64)
import           System.FilePath (takeBaseName)
import           System.IO (hFlush, stdout)
import           Text.Read (readMaybe)

import Schema
import Global
import qualified ES

type DataBlock = [(T.Text, TypeValue V.Vector)]

hdf5ReadVector :: H5.NativeType a => H5.Dataset -> [H5.HSize] -> Word -> IO (V.Vector a)
hdf5ReadVector d o l = do
  s <- H5.getDatasetSpace d
  (b@(b1:_), _) <- H5.getSimpleDataspaceExtent s
  let o1:os = pad o b
      l' | o1 >= b1 = 0
         | otherwise = min (fromIntegral l) $ b1 - o1
  v <- VSM.unsafeNew (fromIntegral l')
  when (l' > 0) $ do
    H5.selectHyperslab s H5.Set ((o1, Nothing, l', Nothing) : map (, Nothing, 1, Nothing) os)
    m <- H5.createSimpleDataspace [l']
    H5.readDatasetInto d (Just m) (Just s) Nothing v
  V.convert <$> VS.unsafeFreeze v
  where
  pad s [] = s
  pad (x:s) (_:n) = x : pad s n
  pad    s  (_:n) = 0 : pad s n -- replicate n 0

hdf5ReadType :: Type -> H5.Dataset -> [H5.HSize] -> Word -> IO (TypeValue V.Vector)
hdf5ReadType (Long    _) d o l = Long    <$> hdf5ReadVector d o l
hdf5ReadType (Integer _) d o l = Integer <$> hdf5ReadVector d o l
hdf5ReadType (Short   _) d o l = Short   <$> hdf5ReadVector d o l
hdf5ReadType (Byte    _) d o l = Byte    <$> hdf5ReadVector d o l
hdf5ReadType (Double  _) d o l = Double  <$> hdf5ReadVector d o l
hdf5ReadType (Float   _) d o l = Float   <$> hdf5ReadVector d o l
hdf5ReadType t           _ _ _ = fail $ "Unsupported HDF5 type: " ++ show t

loadBlock :: Catalog -> Word64 -> Word -> H5.File -> IO DataBlock
loadBlock Catalog{ catalogFields = cat } off len hf = concat <$> mapM (\f ->
    bracket (H5.openDataset hf (TE.encodeUtf8 $ fieldName f) Nothing) H5.closeDataset $ \hd -> do
      let
        loop _ [] = return []
        loop i (f':fs') = do
          x <- hdf5ReadType (fieldType f') hd (fromIntegral off : if i == 0 then [] else [i]) len
          ((fieldName f', x) :) <$> loop (succ i) fs'
      loop 0 $ expandFields (V.singleton f))
  cat

withHDF5 :: FilePath -> (H5.File -> IO a) -> IO a
withHDF5 fn = bracket (H5.openFile (BSC.pack fn) [H5.ReadOnly] Nothing) H5.closeFile

blockLength :: DataBlock -> Int
blockLength = maximum . map (onTypeValue V.length . snd)

ingestBlock :: Catalog -> String -> Word64 -> DataBlock -> M Int
ingestBlock cat@Catalog{ catalogStore = CatalogES{} } pfx off dat = do
  ES.createBulk cat $ map doc [0..pred n]
  return n
  where
  n = blockLength dat
  doc i = (pfx ++ show (off + fromIntegral i), foldMap (\(k, v) -> k J..= fmapTypeValue1 (V.! i) v) dat)
ingestBlock _ _ _ _ = fail "ingestBlock: not implemented"

ingestHDF5 :: Catalog -> String -> M Word64
ingestHDF5 cat fno = liftBaseOp (withHDF5 fn) $ \hf -> do
  let loop o = do
        liftIO $ putStr (show o ++ "\r") >> hFlush stdout
        n <- ingestBlock cat pfx o =<< liftIO (loadBlock cat o blockSize hf)
        (if n < fromIntegral blockSize then return else loop) (o + fromIntegral n)
  loop off
  where
  (fn, off) = splitoff fno
  pfx = takeBaseName fn ++ "_"
  blockSize = 1000
  splitoff [] = ([], 0)
  splitoff ('@':(readMaybe -> Just i)) = ([], i)
  splitoff (c:s) = first (c:) $ splitoff s
