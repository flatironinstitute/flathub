{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.HDF5
  ( ingestHDF5
  ) where

import           Control.Arrow ((&&&))
import           Control.Exception (bracket, handleJust)
import           Control.Monad (foldM, guard, when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Control (liftBaseOp)
import qualified Bindings.HDF5 as H5
import qualified Bindings.HDF5.Error as H5E
import qualified Bindings.HDF5.ErrorCodes as H5E
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toLower)
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity(Identity))
import qualified Data.HashMap.Strict as HM
import           Data.List (find, nub)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word (Word64, Word8)
import           System.IO (hFlush, stdout)

import Type
import Field
import Catalog
import Global
import qualified ES
import Ingest.Types

type DataBlock = [(T.Text, TypeValue V.Vector)]

data IngestFlag
  = IngestAttribute
  | IngestOptional
  | IngestConst
  | IngestIllustris
  | IngestCamels
  | IngestSubhalo
  deriving (Eq, Enum, Bounded, Show)

ingestFlags :: [(T.Text, IngestFlag)]
ingestFlags = map (s . show &&& id) $ enumFromTo minBound maxBound where
  s ~('I':'n':'g':'e':'s':'t':u:r) = T.pack (toLower u : r)

parseIngest :: Field -> (Maybe IngestFlag, T.Text)
parseIngest Field{ fieldName = n, fieldIngest = Nothing } = (Nothing, n)
parseIngest Field{ fieldName = n, fieldIngest = Just (T.stripPrefix "_" -> Just (T.breakOn ":" -> ((`lookup` ingestFlags) -> Just f, s))) } = 
  (Just f, case T.uncons s of
    Nothing -> n
    Just (~':', r) -> r)
parseIngest Field{ fieldIngest = Just i } = (Nothing, i)

hdf5ReadVector :: H5.NativeType a => T.Text -> H5.Dataset -> [H5.HSize] -> Word64 -> IO (V.Vector a)
hdf5ReadVector label d o l = do
  v <- bracket (H5.getDatasetSpace d) H5.closeDataspace $ \s -> do
    (b@(b1:_), _) <- H5.getSimpleDataspaceExtent s
    let o1:os = pad o b
        l' | o1 >= b1 = 0
           | otherwise = min (fromIntegral l) $ b1 - o1
    v <- VSM.unsafeNew (fromIntegral l')
    when (l' > 0) $ do
      H5.selectHyperslab s H5.Set ((o1, Nothing, l', Nothing) : map (, Nothing, 1, Nothing) os)
      bracket (H5.createSimpleDataspace [l']) H5.closeDataspace $ \m ->
        H5.readDatasetInto d (Just m) (Just s) Nothing v
    V.convert <$> VS.unsafeFreeze v
  t <- H5.getDatasetType d
  tc <- H5.getTypeClass t
  ts <- H5.getTypeSize t
  let nt = H5.nativeTypeOf1 v
  ntc <- H5.getTypeClass nt
  nts <- H5.getTypeSize nt
  unless (tc == ntc && ts >= nts) $ fail $ "HDF5 type mismatch: " ++ T.unpack label ++ " " ++ show ((tc, ts), (ntc, nts))
  return v
  where
  pad s [] = s
  pad (x:s) (_:n) = x : pad s n
  pad    s  (_:n) = 0 : pad s n -- replicate n 0

toBool :: Word8 -> Bool
toBool = (0 /=)

hdf5ReadType :: Functor f => Type -> (forall a . H5.NativeType a => IO (f a)) -> IO (TypeValue f)
hdf5ReadType (Long    _) f = Long    <$> f
hdf5ReadType (Integer _) f = Integer <$> f
hdf5ReadType (Short   _) f = Short   <$> f
hdf5ReadType (Byte    _) f = Byte    <$> f
hdf5ReadType (Double  _) f = Double  <$> f
hdf5ReadType (Float   _) f = Float   <$> f
hdf5ReadType (Boolean _) f = Boolean . fmap toBool <$> f
hdf5ReadType t           _ = fail $ "Unsupported HDF5 type: " ++ show t

blockSize :: MonadFail m => DataBlock -> m Int
blockSize dat = case nub $ map (unTypeValue V.length . snd) dat of
  [n] -> return n
  n -> fail $ "Inconsistent data lengths: " ++ show n

loadBlock :: Ingest -> H5.File -> IO DataBlock
loadBlock info@Ingest{ ingestCatalog = Catalog{ catalogFieldGroups = cat }, ingestOffset = off } hf = concat <$> mapM loadf cat where
  loadf f = case parseIngest f of
    (Nothing, "") -> concat <$> mapM loadf (fold $ fieldSub f)
    (Just IngestAttribute, _) -> return []
    (Just IngestConst, _) -> return []
    (Just IngestOptional, n) -> 
      handleJust
        (\(H5E.errorStack -> (H5E.HDF5Error{ H5E.classId = cls, H5E.majorNum = Just H5E.Sym, H5E.minorNum = Just H5E.NotFound }:_)) -> guard (cls == H5E.hdfError))
        (\() -> return [])
      $ loadf f{ fieldIngest = Just n } -- not quite right name handling
    (Just IngestIllustris, _) -> indexf f
    (Just IngestCamels, _) -> indexf f
    (Just IngestSubhalo, _) -> return []
    (Nothing, n) -> withDataset hf n $ \hd -> do
      let
        loop _ [] = return []
        loop i (f':fs') = do
          x <- hdf5ReadType (fieldType f') $ hdf5ReadVector (fieldName f') hd (fromIntegral off : if i == 0 then [] else [i]) $ ingestBlockSize info
          ((fieldName f', x) :) <$> loop (succ i) fs'
      loop 0 $ V.toList $ expandFields (V.singleton f)
  indexf f = return
    [(fieldName f, Long (V.generate (fromIntegral $ maybe id (min . subtract off) (ingestSize info) $ ingestBlockSize info) ((+) (fromIntegral $ ingestStart info + off) . fromIntegral)))]

blockJson :: DataBlock -> Int -> J.Series
blockJson d i = foldMap (\(k, v) -> k J..= fmapTypeValue1 (V.! i) v) d

blockDoc :: Ingest -> J.Series -> DataBlock -> Int -> (String, J.Series)
blockDoc info e d i =
  ( ingestPrefix info ++ show (ingestStart info + ingestOffset info + fromIntegral i)
  , ingestJConsts info <> e <> blockJson d i)

ingestWith :: Ingest -> (Int -> (String, J.Series)) -> Int -> M Int
ingestWith Ingest{ ingestCatalog = cat } doc n = do
  when (n /= 0) $ ES.createBulk cat docs
  return n
  where
  docs = map doc [0..pred n]

ingestBlock :: Ingest -> DataBlock -> M Int
ingestBlock info dat =
  ingestWith info (blockDoc info mempty dat) =<< blockSize dat

ingestSubBlocks :: Ingest -> H5.File -> DataBlock -> IngestJoin -> M Int
ingestSubBlocks info hf pb IngestHaloJoin{..} = do
  fcl <- V.zip <$> getcol joinFirst pb <*> getcol joinCount pb
  let fcl' = V.filter ((-1 /=) . fst) fcl
  if V.null fcl' then return 0 else do
    let (off, _) = V.head fcl'
        (lo, lc) = V.last fcl'
        n = lo + lc - off
    unless (fromIntegral off == ingestOffset joinIngest) $ fail $ "suboffset missmatch: " ++ show off ++ " /= " ++ show (ingestOffset joinIngest)
    sb <- liftIO $ loadBlock joinIngest{ ingestBlockSize = fromIntegral n } hf
    sn <- blockSize sb
    unless (sn == fromIntegral n) $ fail $ "Incorrect subblock length: " ++ show sn ++ "/" ++ show n
    pbi <- getcol joinParent sb
    let doc i = blockDoc joinIngest (blockJson pb (fromIntegral (fromIntegral (pbi V.! i) - ingestOffset info))) sb i
    ingestWith info doc sn
  where
  getcol f b = case lookup f b of
    Just (Integer v) -> return v
    _ -> fail $ "ingest join data not found for " ++ show f

withHDF5 :: FilePath -> (H5.File -> IO a) -> IO a
withHDF5 fn = bracket (H5.openFile (BSC.pack fn) [H5.ReadOnly] Nothing) H5.closeFile

withDataset :: H5.File -> T.Text -> (H5.Dataset -> IO a) -> IO a
withDataset hf n = bracket
  (H5.openDataset hf (TE.encodeUtf8 n) Nothing)
  H5.closeDataset

withAttribute :: H5.File -> T.Text -> (H5.Attribute -> IO a) -> IO a
withAttribute hf p = bracket
  (if BSC.null g then H5.openAttribute hf a else bracket
    (H5.openGroup hf g Nothing)
    H5.closeGroup $ \hg -> H5.openAttribute hg a)
  H5.closeAttribute
  where (g, a) = BSC.breakEnd ('/'==) (TE.encodeUtf8 p)

readAttribute :: H5.File -> T.Text -> Type -> IO (TypeValue V.Vector)
readAttribute hf p t = withAttribute hf p $ \a ->
  hdf5ReadType t $ VS.convert <$> H5.readAttribute a

readScalarValue :: T.Text -> TypeValue V.Vector -> Value
readScalarValue p r
  | unTypeValue V.length r == 1 = fmapTypeValue1 V.head r
  | otherwise = error $ "non-scalar const value: " ++ show p

readScalarAttribute :: H5.File -> T.Text -> Type -> IO Value
readScalarAttribute hf p t =
  readScalarValue p <$> readAttribute hf p t

ingestIncr :: Int -> Ingest -> Ingest
ingestIncr n i = i{ ingestOffset = ingestOffset i + fromIntegral n }

ingestDone :: MonadFail m => Ingest -> m ()
ingestDone i =
  unless (all (ingestOffset i ==) $ ingestSize i) $ fail $ "size mismatch: expected " ++ show (ingestSize i) ++ " rows, got " ++ show (ingestOffset i)

ingestHFile :: Ingest -> H5.File -> M Word64
ingestHFile info hf = do
  info' <- foldM infof info (catalogFieldGroups $ ingestCatalog info)
  if ingestSize info' == Just 0 -- for illustris, if there's no data, don't try reading (since datasets are missing)
    then return 0
    else loop info'
  where
  infof i f = case parseIngest f of
    (Just IngestAttribute, n) -> do
      v <- liftIO $ readScalarAttribute hf n (fieldType f)
      return $ addIngestConsts (setFieldValue f v) i
    (Just IngestConst, n) -> do
      v <- liftIO $ readScalarValue n <$> withDataset hf n (\hd -> hdf5ReadType (fieldType f) $ hdf5ReadVector n hd [] 2)
      return $ addIngestConsts (setFieldValue f v) i
    (Just IngestIllustris, ill) -> do
      sz <- getIllustrisSize ill
      si <- constv "simulation" i
      sn <- constv "snapshot" i
      return i
        { ingestPrefix = si ++ '_' : sn ++ "_"
        , ingestSize = Just (fromIntegral sz) }
    (Just IngestCamels, ill) -> do
      sz <- getIllustrisSize ill
      ssuite <- constv "simulation_suite" i
      FieldValue Field{ fieldEnum = Just ssete } (Byte ssetv) <- constf "simulation_set" i
      sid <- constv "simulation_set_id" i
      sn <- constv "snapshot" i
      return i
        { ingestPrefix = ssuite ++ T.unpack (ssete V.! fromIntegral ssetv) ++ sid ++ '_' : sn ++ [T.head ill]
        , ingestSize = Just (fromIntegral sz) }
    (Just IngestSubhalo, T.words -> ft : fl@[ff, fc, fp]) -> do
      (tf, cat') <- maybe (fail "missing join type field") return $ takeCatalogField ft (ingestCatalog i)
      unless (all (`HM.member` catalogFieldMap (ingestCatalog i)) fl) $ fail "missing join fields"
      let tfv x = addIngestConsts $ setFieldValue tf (Boolean (Identity x))
          fg = fold $ fieldSub f
      si <- foldM infof (tfv True i)
        { ingestCatalog = cat'{ catalogFieldGroups = fg }
        , ingestSize = Nothing
        , ingestStart = 0
        } fg
      return $ tfv False i
        { ingestCatalog = cat'
        , ingestJoin = Just $ IngestHaloJoin si ff fc fp
        }
    _ -> return i -- XXX only top-level ingest flags processed here
  getIllustrisSize ill = liftIO $ do
    Long sz <- readScalarAttribute hf ("Header/N" <> case ill of { "Subhalo" -> "subgroup" ; s -> T.toLower s } <> "s_ThisFile") (Long Proxy)
    handleJust
      (\(H5E.errorStack -> (H5E.HDF5Error{ H5E.classId = cls, H5E.majorNum = Just H5E.Attr, H5E.minorNum = Just H5E.NotFound }:_)) -> guard (cls == H5E.hdfError)) return $ do
        Long fi <- readScalarAttribute hf "Header/Num_ThisFile" (Long Proxy)
        Long ids <- readAttribute hf ("Header/FileOffsets_" <> ill) (Long Proxy)
        let si = fromIntegral (ids V.! fromIntegral fi)
        unless (si == ingestStart info) $ fail "start offset mismatch"
    return sz
  constv f i = show . fieldValue <$> constf f i
  constf f = maybe (fail $ "const field " ++ show f ++ " not found")
    return . find ((f ==) . fieldName . fieldDesc) . ingestConsts
  loop i = do
    b <- liftIO (loadBlock i hf)
    n <- ingestBlock i b
    ij <- mapM (\ij -> do
      nj <- if n == 0 then return 0 else ingestSubBlocks i hf b ij
      return $ ij{ joinIngest = ingestIncr nj (joinIngest ij) })
      $ ingestJoin i
    let i' = ingestIncr n i{ ingestJoin = ij }
    liftIO $ putStr (show (ingestOffset i') ++ ' ' : foldMap (show . ingestOffset . joinIngest) ij ++ "\r") >> hFlush stdout
    if n < fromIntegral (ingestBlockSize i)
      then do
        ingestDone i'
        mapM_ (ingestDone . joinIngest) ij
        return $ ingestOffset i'
      else loop i'

ingestHDF5 :: Ingest -> M Word64
ingestHDF5 info = liftBaseOp (withHDF5 $ ingestFile info) $ ingestHFile info
