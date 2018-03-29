{-# LANGUAGE TupleSections #-}

module Ingest
  ( loadHDF5
  ) where

import           Control.Exception (bracket)
import           Control.Monad (foldM)
import qualified Bindings.HDF5 as H5
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Schema

type DataBlock = HM.HashMap T.Text (TypeValue VS.Vector)

hdf5ReadVector :: H5.NativeType a => H5.Dataset -> [H5.HSize] -> Word -> IO (VS.Vector a)
hdf5ReadVector d o l = do
  m <- H5.createSimpleDataspace [fromIntegral l]
  s <- H5.getDatasetSpace d
  n <- H5.getSimpleDataspaceExtentNDims s
  let o1:os = pad o n
  H5.selectHyperslab s H5.Set ((o1, Nothing, fromIntegral l, Nothing) : map (, Nothing, 1, Nothing) os)
  v <- VSM.unsafeNew (fromIntegral l)
  H5.readDatasetInto d (Just m) (Just s) Nothing v
  VS.unsafeFreeze v
  where
  pad s 0 = s
  pad (x:s) n = x : pad s (pred n)
  pad    s  n = 1 : pad s (pred n) -- replicate n 1

hdf5ReadType :: Type -> H5.Dataset -> [H5.HSize] -> Word -> IO (TypeValue VS.Vector)
hdf5ReadType (Long    _) d o l = Long    <$> hdf5ReadVector d o l
hdf5ReadType (Integer _) d o l = Integer <$> hdf5ReadVector d o l
hdf5ReadType (Short   _) d o l = Short   <$> hdf5ReadVector d o l
hdf5ReadType (Byte    _) d o l = Byte    <$> hdf5ReadVector d o l
hdf5ReadType (Double  _) d o l = Double  <$> hdf5ReadVector d o l
hdf5ReadType (Float   _) d o l = Float   <$> hdf5ReadVector d o l
hdf5ReadType t           _ _ _ = fail $ "Unsupported HDF5 type: " ++ show t

loadHDF5 :: Catalog -> FilePath -> Word -> Word -> IO DataBlock
loadHDF5 Catalog{ catalogFields = cat } fn off len =
  bracket (H5.openFile (BSC.pack fn) [H5.ReadOnly] Nothing) H5.closeFile $ \hf ->
    foldM (\h f ->
      bracket (H5.openDataset hf (TE.encodeUtf8 $ fieldName f) Nothing) H5.closeDataset $ \hd -> do
        let
          loop _ [] h' = return h'
          loop i (f':fs') h' = do
            x <- hdf5ReadType (fieldType f') hd (fromIntegral off : if i == 0 then [] else [i]) len
            loop (succ i) fs' $ HM.insert (fieldName f') x h'
        loop 0 (expandFields (V.singleton f)) h)
      HM.empty cat
