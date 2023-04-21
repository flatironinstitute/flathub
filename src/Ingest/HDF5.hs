{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.HDF5
  ( ingestHDF5
  , ingestTNG
  ) where

import           Control.Arrow ((&&&), first)
import           Control.Exception (bracket, handleJust)
import           Control.Monad (foldM, guard, when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Control (liftBaseOp)
import qualified Bindings.HDF5 as H5
import qualified Bindings.HDF5.Error as H5E
import qualified Bindings.HDF5.ErrorCodes as H5E
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (toLower)
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity(Identity))
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import           Data.List (find, nub)
import           Data.Maybe (fromJust, isNothing, fromMaybe)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word (Word64, Word8)
import           System.FilePath ((</>))
import           System.IO (hFlush, stdout)
import           Text.Printf (printf)

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
  | IngestSupplemental -- TNG/EAGLE
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
  unless ((tc == ntc || tc == H5.Enum && ntc == H5.Integer) && ts >= nts) $
    fail $ "HDF5 type mismatch: " ++ T.unpack label ++ " " ++ show ((tc, ts), (ntc, nts))
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

readBlock :: Ingest -> H5.File -> IO DataBlock
readBlock info@Ingest{ ingestCatalog = Catalog{ catalogFieldGroups = cat }, ingestOffset = off, ingestBlockSize = n } hf = loadfs cat where
  loadfs f = concat <$> mapM loadf f
  loadf f = case parseIngest f of
    (Nothing, "") -> loadfs (fold $ fieldSub f)
    (Just IngestAttribute, _) -> return []
    (Just IngestConst, _) -> return []
    (Just IngestOptional, fn) -> 
      handleJust
        (\(H5E.errorStack -> (H5E.HDF5Error{ H5E.classId = cls, H5E.majorNum = Just H5E.Sym, H5E.minorNum = Just H5E.NotFound }:_)) -> guard (cls == H5E.hdfError))
        (\() -> return [])
      $ loadf f{ fieldIngest = Just fn } -- not quite right name handling
    (Just IngestIllustris, _) -> indexf f
    (Just IngestCamels, _) -> indexf f
    (Just IngestSubhalo, _) -> return []
    (Just IngestSupplemental, _) -> return []
    (Nothing, fn) -> withDataset hf fn $ \hd -> do
      let
        loop _ [] = return []
        loop i (f':fs') = do
          x <- hdf5ReadType (fieldType f') $ hdf5ReadVector (fieldName f') hd (fromIntegral off : if i == 0 then [] else [i]) n
          ((fieldName f', x) :) <$> loop (succ i) fs'
      loop 0 $ V.toList $ expandFields (V.singleton f)
  indexf f = return
    [(fieldName f, Long (V.generate (fromIntegral $ fromMaybe n expn) ((+) (fromIntegral $ ingestPos info) . fromIntegral)))]
  expn = min n . subtract off <$> ingestSize info

blockSize :: MonadFail m => DataBlock -> m Int
blockSize dat = case nub $ map (unTypeValue V.length . snd) dat of
  [n] -> return n
  n -> fail $ "Inconsistent data lengths: " ++ show n

loadBlock :: Ingest -> H5.File -> IO (Int, DataBlock, Ingest)
loadBlock i hf = do
  b <- readBlock i hf
  n <- blockSize b
  let i' = ingestIncr n i
  return (n, b,
    if n < fromIntegral (ingestBlockSize i) && isNothing (ingestSize i')
      then i'{ ingestSize = Just (ingestOffset i') }
      else i')

blockJson :: DataBlock -> Int -> J.Series
blockJson d i = foldMap (\(k, v) -> JK.fromText k J..= fmapTypeValue1 (V.! i) v) d

blockDoc :: Ingest -> J.Series -> DataBlock -> Int -> (String, J.Series)
blockDoc info e d i =
  ( ingestPrefix info ++ show (ingestPos info + fromIntegral i)
  , ingestJConsts info <> e <> blockJson d i)

ingestWith :: Ingest -> (Int -> (String, J.Series)) -> Int -> M ()
ingestWith Ingest{ ingestCatalog = cat } doc n = do
  when (n /= 0) $ ES.createBulk cat docs
  where
  docs = map doc [0..pred n]

ingestBlock :: Ingest -> DataBlock -> Int -> M ()
ingestBlock info dat n =
  ingestWith info (blockDoc info mempty dat) n

type FileStack = [(Ingest, H5.File)]

loadStackBlock :: FileStack -> Word64 -> IO (Int, DataBlock, FileStack)
loadStackBlock ((i, h):r) m
  | Just (ingestOffset i) == ingestSize i = loadStackBlock r m
  | otherwise = do
    (n, b, i') <- loadBlock i{ ingestBlockSize = min m (ingestBlockSize i) } h
    return (n, b, (i'{ ingestBlockSize = ingestBlockSize i }, h):r)
loadStackBlock [] _ = return (0, [], [])

ingestSubStackBlocks :: Ingest -> DataBlock -> FileStack -> Maybe (IM.IntMap Int, Fields, H5.File) -> M FileStack
ingestSubStackBlocks info@Ingest{ ingestJoin = Just IngestHaloJoin{..} } pb shfs supp = do
  fcl <- V.zip <$> getcol joinFirst pb <*> getcol joinCount pb
  let fcl' = V.filter ((-1 /=) . fst) fcl
  if V.null fcl' then return shfs else do
    let (off, _) = V.head fcl'
        (lo, lc) = V.last fcl'
        n = lo + lc - off
    unless (n >= 0 && fromIntegral off == ingestPos (fst (head shfs)))
      $ fail $ "suboffset missmatch: " ++ show off ++ " /= " ++ show (ingestPos (fst (head shfs)))
    loop (fromIntegral n) shfs
  where
  loop 0 hfs = return hfs
  loop n hfs@((jinfo, _):_) = do
    (sn, sb, hfs') <- liftIO $ loadStackBlock hfs n
    unless (sn >= 0) $ fail "missing subdata"
    pbi <- getcol joinParent sb
    sups <- liftIO $ mapM (\(idx, fl, hf) -> (idx, ) <$>
      mapM (\(supo, supn) -> (supo, ) <$>
        readBlock jinfo
          { ingestCatalog = (ingestCatalog jinfo){ catalogFieldGroups = fl }
          , ingestStart = 0
          , ingestOffset = fromIntegral supo
          , ingestBlockSize = fromIntegral supn
          } hf)
        (do
          (_, l) <- IM.lookupGE so idx
          (_, r) <- IM.lookupLT (so + sn) idx
          (l, r-l+1) <$ guard (l <= r))
      ) supp
    let doc i = blockDoc jinfo
          (blockJson pb (fromIntegral (pbi V.! i) - fromIntegral (ingestPos info)) <>
           foldMap (supdoc i) sups)
          sb i
    ingestWith info doc sn
    loop (n - fromIntegral sn) hfs'
    where
    so = fromIntegral $ ingestPos jinfo
    supdoc i (supi, Just (supo, supb)) = 
      foldMap (blockJson supb . subtract supo) (supi IM.!? (so + i))
    supdoc _ _ = mempty
  getcol f b = case lookup f b of
    Just (Integer v) -> return v
    _ -> fail $ "ingest join data not found for " ++ show f
ingestSubStackBlocks _ _ _ _ = fail "ingestSubStackBlocks: unsupported join"

ingestSubBlocks :: Ingest -> DataBlock -> H5.File -> M (Maybe IngestJoin)
ingestSubBlocks info pb hf = mapM (\ij -> do
  [(i, _)] <- ingestSubStackBlocks info pb [(joinIngest ij, hf)] Nothing
  return ij{ joinIngest = i }) $ ingestJoin info

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

constField :: MonadFail m => T.Text -> Ingest -> m FieldValue
constField f = maybe (fail $ "const field " ++ show f ++ " not found")
  return . find ((f ==) . fieldName . fieldDesc) . ingestConsts

addSupplemental :: Field -> Maybe IngestJoin -> Maybe IngestJoin
addSupplemental f Nothing = addSupplemental f $ Just (IngestJoin V.empty)
addSupplemental f (Just (IngestJoin v)) =
  Just $ IngestJoin $ V.snoc v f
addSupplemental _ _ = fail $ "addSupplemental: incompatible join"

-- |invert from indices to values for a monotonic vector, e.g. 7 [0,2,3,5] -> [0,-1,1,2,-1,3,-1]
invertVectorAsc :: V.Vector Int32 -> (IM.IntMap Int)
invertVectorAsc = IM.fromDistinctAscList . (`zip` [0..]) . map fromIntegral . V.toList
-- invertVectorAsc n v = V.unfoldrN n (\(i, j) -> fmap (\vj -> if vj == i then (j, (succ i, succ j)) else (-1, (succ i, j))) (v V.!? j)) (0, 0)

makeJoinIndex :: Int -> H5.File -> T.Text -> IO (IM.IntMap Int)
makeJoinIndex ntot hf fn =
  handleJust
    (\(H5E.errorStack -> (H5E.HDF5Error{ H5E.classId = cls, H5E.majorNum = Just H5E.Sym, H5E.minorNum = Just H5E.NotFound }:_)) -> IM.empty <$ guard (cls == H5E.hdfError)) return $ do
    invertVectorAsc <$> withDataset hf fn (\hd -> hdf5ReadVector fn hd [] (fromIntegral ntot))

prepareIngest :: Ingest -> H5.File -> M Ingest
prepareIngest info hf = infofs info (catalogFieldGroups $ ingestCatalog info)
  where
  infofs = foldM infof
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
        { ingestPrefix = si ++ '_' : sn ++ [T.head ill]
        , ingestSize = Just (fromIntegral sz) }
    (Just IngestCamels, ill) -> do
      sz <- getIllustrisSize ill
      ssuite <- constv "simulation_suite" i
      FieldValue Field{ fieldEnum = Just ssete } (Byte ssetv) <- constField "simulation_set" i
      sid <- constv "simulation_set_id" i
      sn <- constv "snapshot" i
      return i
        { ingestPrefix = ssuite ++ T.unpack (ssete V.! fromIntegral ssetv) ++ sid ++ '_' : sn ++ [T.head ill]
        , ingestSize = Just (fromIntegral sz) }
    (Just IngestSubhalo, T.words -> ft : fl@[ff, fc, fp]) -> do
      (tf, cat') <- maybe (fail $ "missing join type field: " <> show ft) return $ takeCatalogField ft (ingestCatalog i)
      unless (all (`HM.member` catalogFieldMap (ingestCatalog i)) fl) $ fail "missing join fields"
      let tfv x = addIngestConsts $ setFieldValue tf (Boolean (Identity x))
          fg = fold $ fieldSub f
          ijt = maybe i{ ingestSize = Nothing, ingestStart = 0 }
            joinIngest $ ingestJoin i
      si <- infofs (tfv True ijt)
        { ingestCatalog = cat'{ catalogFieldGroups = fg }
        , ingestFile = ingestFile i
        } fg
      return $ tfv False i
        { ingestCatalog = cat'
        , ingestJoin = Just $ IngestHaloJoin si ff fc fp
        }
    (Just IngestSupplemental, gf) -> do
      return i
        { ingestJoin = addSupplemental
          f{ fieldIngest = Just gf } $ ingestJoin i
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
  constv f i = show . fieldValue <$> constField f i

loadHFile :: Ingest -> H5.File -> M Word64
loadHFile info hf =
  if ingestSize info == Just 0 -- for illustris, if there's no data, don't try reading (since datasets are missing)
    then return 0
    else loop info
  where
  loop i = do
    (n, b, i') <- liftIO $ loadBlock i hf
    ingestBlock i b n
    ij <- ingestSubBlocks i b hf
    let i'' = i'{ ingestJoin = ij }
    liftIO $ putStr (show (ingestOffset i'') ++ ' ' : foldMap (show . ingestOffset . joinIngest) ij ++ "\r") >> hFlush stdout
    if (ingestOffset i'' <) `all` ingestSize i''
      then loop i''
      else maybe
        (return $ ingestOffset i'')
        (\ji -> do
          unless (Just (ingestOffset ji) == ingestSize ji) $
            fail $ "subsize mismatch: " ++ show (ingestOffset ji) ++ "/" ++ show (ingestSize ji)
          return $ ingestOffset i'' + ingestOffset ji)
        (joinIngest <$> ij)

ingestHFile :: Ingest -> H5.File -> M Word64
ingestHFile info hf = do
  info' <- prepareIngest info hf
  loadHFile info' hf

ingestHDF5 :: Ingest -> M Word64
ingestHDF5 info = liftBaseOp (withHDF5 $ ingestFile info) $ ingestHFile info

ingestTNG :: Ingest -> M Word64
ingestTNG inginfo = do
  FieldValue Field{ fieldEnum = Just sime } (Byte sim) <- constField "simulation" inginfo
  FieldValue _ (Short (Identity snap)) <- constField "snapshot" inginfo
  let simn = sime V.! fromIntegral sim
      dir = ingestFile inginfo </> T.unpack simn
      gdir = dir </> "groups"
      isdm = "-Dark" `T.isSuffixOf` simn
      snap3 = printf "%03d" snap
      next Ingest{ ingestStart = s, ingestSize = Just z, ingestOffset = o, ingestJoin = j } = inginfo
        { ingestStart = s + z
        , ingestSize = Nothing
        , ingestOffset = if o > z then o - z else 0
        , ingestJoin = fmap nextj j
        }
      nextj ij@IngestHaloJoin{ joinIngest = i } = ij{ joinIngest = next i }
      nextj ij = ij
      showing i = ingestFile i <> "@" <> show (ingestStart i) <> ":" <> show (ingestOffset i) <> "/" <> show (ingestSize i)
      -- open all files and pass FileStack to act
      open nf fi info act
        | fi == nf = act []
        | otherwise = liftBaseOp (withHDF5 fn) $ \ghf -> do
          Integer (Identity nf') <- liftIO $ readScalarAttribute ghf "Header/NumFiles" (Integer Proxy)
          info' <- prepareIngest info{ ingestFile = fn } ghf
          when (fi /= 0 && nf /= nf') $ fail "NumFiles mismatch"
          open nf' (succ fi) (next info') $ act . ((info', ghf) :)
          where fn = gdir </> ("groups_" ++ snap3) </> ("fof_subhalo_tab_" ++ snap3 ++ "." ++ show fi ++ ".hdf5")
      load [] _ _ = return 0 -- FIXME total rows
      load ghfs@((ginfo, _):_) subhfs supji = do
        liftIO $ putStrLn $ showing ginfo <> " " <> showing (fst $ head subhfs)
        (gn, gb, ghfs') <- liftIO $ loadStackBlock ghfs (ingestBlockSize ginfo)
        subhfs' <- if gn == 0
          then return subhfs -- should be done
          else do
            ingestBlock ginfo gb gn
            ingestSubStackBlocks ginfo gb subhfs supji
        load ghfs' subhfs' supji
  open (-1) 0 inginfo $ \ghfs -> do
    let subhfs@((sinfo, shf):_) = map (first $ joinIngest . fromJust . ingestJoin) ghfs
        start = load ghfs subhfs
    case ingestJoin sinfo of
      Just (IngestJoin supf) | not isdm ->
        liftBaseOp (withHDF5 $ dir </> "supplemental" </> ("Snapshot_" ++ show snap ++ ".hdf5")) $ \suphf -> do
          Integer (Identity nsub) <- liftIO $ readScalarAttribute shf "Header/Nsubgroups_Total" (Integer Proxy)
          supji <- liftIO $ makeJoinIndex (fromIntegral nsub) suphf "Subhalo/GalaxyID"
          start $ Just (supji, supf, suphf)
      Nothing -> start Nothing
