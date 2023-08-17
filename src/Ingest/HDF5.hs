{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Ingest.HDF5
  ( ingestHDF5
  , ingestTNG
  , ingestEagle
  ) where

import           Control.Arrow ((&&&), first, second)
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
import           Data.Default (def)
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

rePutStr :: String -> IO ()
rePutStr = putStr . ("\r\ESC[K" ++)

unStorableValue :: (forall a . (Typed a, VS.Storable a) => VS.Vector a -> b) -> TypeValue VS.Vector -> b
unStorableValue f (Double    x) = f $! x
unStorableValue f (Float     x) = f $! x
unStorableValue f (HalfFloat x) = f $! x
unStorableValue f (Long      x) = f $! x
unStorableValue f (ULong     x) = f $! x
unStorableValue f (Integer   x) = f $! x
unStorableValue f (Short     x) = f $! x
unStorableValue f (Byte      x) = f $! x
unStorableValue f (Boolean   x) = f $! x
unStorableValue _ (Keyword   _) = error "unStorableValue Keyword"
unStorableValue _ (Array     _) = error "unStorableValue Array"
unStorableValue _ (Void      _) = error "unStorableValue Void"

fmapStorableValue1 :: (forall a . (Typed a, VS.Storable a) => VS.Vector a -> a) -> TypeValue VS.Vector -> Value
fmapStorableValue1 f (Double    x) = Double    $ Identity $ f $! x
fmapStorableValue1 f (Float     x) = Float     $ Identity $ f $! x
fmapStorableValue1 f (HalfFloat x) = HalfFloat $ Identity $ f $! x
fmapStorableValue1 f (Long      x) = Long      $ Identity $ f $! x
fmapStorableValue1 f (ULong     x) = ULong     $ Identity $ f $! x
fmapStorableValue1 f (Integer   x) = Integer   $ Identity $ f $! x
fmapStorableValue1 f (Short     x) = Short     $ Identity $ f $! x
fmapStorableValue1 f (Byte      x) = Byte      $ Identity $ f $! x
fmapStorableValue1 f (Boolean   x) = Boolean   $ Identity $ f $! x
fmapStorableValue1 _ (Keyword   _) = error "fmapStorableValue Keyword"
fmapStorableValue1 _ (Array     _) = error "fmapStorableValue Array"
fmapStorableValue1 _ (Void      _) = error "fmapStorableValue Void"

type DataOf f = [(T.Text, TypeValue f)]
type DataValues = DataOf Identity
type DataBlock = DataOf VS.Vector

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

hdf5ReadVector :: H5.NativeType a => T.Text -> H5.Dataset -> [H5.HSize] -> Word64 -> IO (VS.Vector a)
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

hdf5ReadType :: Type -> (forall a . H5.NativeType a => IO (VS.Vector a)) -> IO (TypeValue VS.Vector)
hdf5ReadType (Long    _) f = Long    <$> f
hdf5ReadType (Integer _) f = Integer <$> f
hdf5ReadType (Short   _) f = Short   <$> f
hdf5ReadType (Byte    _) f = Byte    <$> f
hdf5ReadType (Double  _) f = Double  <$> f
hdf5ReadType (Float   _) f = Float   <$> f
hdf5ReadType (Boolean _) f = Boolean . VS.map toBool <$> f
hdf5ReadType t           _ = fail $ "Unsupported HDF5 type: " ++ show t

readBlock :: H5.Location hl => Ingest -> hl -> IO DataBlock
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
    [(fieldName f, Long (VS.generate (fromIntegral $ fromMaybe n expn) ((+) (fromIntegral $ ingestPos info) . fromIntegral)))]
  expn = min n . subtract off <$> ingestSize info

blockSize :: MonadFail m => DataBlock -> m Int
blockSize dat = case nub $ map (unStorableValue VS.length . snd) dat of
  [n] -> return n
  n -> fail $ "Inconsistent data lengths: " ++ show n

loadBlock :: H5.Location hl => Ingest -> hl -> IO (Int, DataBlock, Ingest)
loadBlock i hf = do
  b <- readBlock i hf
  n <- blockSize b
  let i' = ingestIncr n i
  return (n, b,
    if n < fromIntegral (ingestBlockSize i) && isNothing (ingestSize i')
      then i'{ ingestSize = Just (ingestOffset i') }
      else i')

blockJsonWith :: (TypeValue f -> Value) -> DataOf f -> J.Series
blockJsonWith f d = foldMap (\(k, v) -> JK.fromText k J..= f v) d

blockIndex :: Int -> TypeValue VS.Vector -> Value
blockIndex i = fmapStorableValue1 (VS.! i)

blockJson :: DataBlock -> Int -> J.Series
blockJson d i = blockJsonWith (blockIndex i) d

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
  loop _ [] = fail "ingestSubStacBlocks loop"
  getcol :: T.Text -> DataBlock -> M (V.Vector Int32)
  getcol f b = case lookup f b of
    Just (Integer v) -> return $ V.convert v
    _ -> fail $ "ingest join data not found for " ++ show f
ingestSubStackBlocks _ _ _ _ = fail "ingestSubStackBlocks: unsupported join"

ingestSubBlocks :: Ingest -> DataBlock -> H5.File -> M (Maybe IngestJoin)
ingestSubBlocks info pb hf = mapM (\ij -> do
  [(i, _)] <- ingestSubStackBlocks info pb [(joinIngest ij, hf)] Nothing
  return ij{ joinIngest = i }) $ ingestJoin info

withHDF5 :: FilePath -> (H5.File -> IO a) -> IO a
withHDF5 fn = bracket (H5.openFile (BSC.pack fn) [H5.ReadOnly] Nothing) H5.closeFile

withDataset :: H5.Location hl => hl -> T.Text -> (H5.Dataset -> IO a) -> IO a
withDataset hf n = bracket
  (H5.openDataset hf (TE.encodeUtf8 n) Nothing)
  H5.closeDataset

withGroup :: H5.File -> T.Text -> (H5.Group -> IO a) -> IO a
withGroup h5 g = bracket
  (H5.openGroup h5 (TE.encodeUtf8 g) Nothing)
  H5.closeGroup

withGroupMaybe :: H5.File -> T.Text -> (Maybe H5.Group -> IO a) -> IO a
withGroupMaybe h5 g = bracket
  (handleJust
    (\(H5E.errorStack -> (H5E.HDF5Error{ H5E.classId = cls, H5E.majorNum = Just H5E.Sym, H5E.minorNum = Just H5E.NotFound }:_)) -> Nothing <$ guard (cls == H5E.hdfError)) return $
      Just <$> H5.openGroup h5 (TE.encodeUtf8 g) Nothing)
  (mapM_ H5.closeGroup)

withAttribute :: H5.File -> T.Text -> (H5.Attribute -> IO a) -> IO a
withAttribute hf p = bracket
  (if T.null g then H5.openAttribute hf as else withGroup hf g
    $ \hg -> H5.openAttribute hg as)
  H5.closeAttribute
  where
  (g, a) = T.breakOnEnd "/" p
  as = TE.encodeUtf8 a

readAttribute :: H5.File -> T.Text -> Type -> IO (TypeValue VS.Vector)
readAttribute hf p t = withAttribute hf p $ \a ->
  hdf5ReadType t $ VS.convert <$> H5.readAttribute a

readScalarValue :: T.Text -> TypeValue VS.Vector -> Value
readScalarValue p r
  | unStorableValue VS.length r == 1 = fmapStorableValue1 VS.head r
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
invertVectorAsc :: VS.Vector Int32 -> (IM.IntMap Int)
invertVectorAsc = IM.fromDistinctAscList . (`zip` [0..]) . map fromIntegral . VS.toList
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
        let si = fromIntegral (ids VS.! fromIntegral fi)
        unless (si == ingestStart info) $ fail "start offset mismatch"
    return sz
  constv f i = show . fieldValue <$> constField f i

ingestEOF :: Ingest -> Bool
ingestEOF i = (ingestOffset i >=) `any` ingestSize i

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
    liftIO $ rePutStr (show (ingestOffset i'') ++ ' ' : foldMap (show . ingestOffset . joinIngest) ij) >> hFlush stdout
    if ingestEOF i''
      then maybe
        (return $ ingestOffset i'')
        (\ji -> do
          unless (Just (ingestOffset ji) == ingestSize ji) $
            fail $ "subsize mismatch: " ++ show (ingestOffset ji) ++ "/" ++ show (ingestSize ji)
          return $ ingestOffset i'' + ingestOffset ji)
        (joinIngest <$> ij)
      else loop i''

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
      isill = T.hasPrefix "Illustris" simn
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
          where fn = gdir </> ("groups_" ++ snap3) </> ((if isill then "fof_subhalo_tab_" else "groups_") ++ snap3 ++ "." ++ show fi ++ ".hdf5")
      load [] _ _ = return 0 -- FIXME total rows
      load ghfs@((ginfo, _):_) subhfs supji = do
        liftIO $ rePutStr $ showing ginfo <> " " <> showing (fst $ head subhfs)
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
      _ -> start Nothing

data EagleSub = EagleSub
  { eagleName :: T.Text
  , eagleGroup :: H5.Group
  , eagleIngest :: Ingest
  , eagleBlock :: DataBlock
  , eagleOff, eagleLen :: Int
  , eagleKey :: VS.Vector Int32
  , eagleMap :: IM.IntMap Int
  , eagleTake :: Int32 -> EagleSub -> IO (Maybe DataValues, EagleSub)
  } | EagleBlock
  { eagleBlocks :: [EagleSub]
  , eagleTake :: Int32 -> EagleSub -> IO (Maybe DataValues, EagleSub)
  }

eagleKeyField, eagleApertureField :: T.Text
eagleKeyField = "GalaxyID"
eagleApertureField = "ApertureSize"

eagleNew :: T.Text -> T.Text -> Ingest -> H5.Group -> EagleSub
eagleNew simn name info@Ingest{ ingestCatalog = cat } hg
  | Just bs <- isblock = EagleBlock
  { eagleBlocks = map (\b -> eagleNew (simn <> "_" <> T.pack (show b)) name info
    { ingestOffset = bs * b
    , ingestSize = Just (bs * succ b)
    } hg) [0..9]
  , eagleTake = tke
  }
  | otherwise = EagleSub
  { eagleName = name
  , eagleGroup = hg
  , eagleIngest = info{ ingestCatalog = cat
    { catalogFieldGroups = def
      { fieldName = eagleKeyField
      , fieldType = Integer Proxy
      } `V.cons` (if isap then unap else id) (catalogFieldGroups cat)
    } }
  , eagleBlock = []
  , eagleOff = 0
  , eagleLen = 0
  , eagleKey = VS.empty
  , eagleMap = IM.empty
  , eagleTake = tke
  } where
  tke = if isap then eagleAperture else if ismap then eagleInitMap else eagleTake1
  isap = name == "Aperture"
  isblock = 65996151 <$ guard (isap && simn == "RefL0100N1504")
  ismap = name == "Magnitudes"
  unap f = def
    { fieldName = eagleApertureField
    , fieldType = Integer Proxy
    } `V.cons` V.map unsub f
  unsub f = f{ fieldSub = Nothing }

eagleLoad :: EagleSub -> IO EagleSub
eagleLoad e
  | eagleOff e < eagleLen e || ingestEOF (eagleIngest e) = return e
  | otherwise = do
  (n, (kn,Integer kb):b, i) <- loadBlock (eagleIngest e) (eagleGroup e)
  unless (kn == eagleKeyField) $ fail $ "eagleLoad " <> T.unpack kn
  return e
    { eagleIngest = i
    , eagleBlock = b
    , eagleOff = 0
    , eagleLen = n
    , eagleKey = kb
    }

eagleInitMap :: Int32 -> EagleSub -> IO (Maybe DataValues, EagleSub)
eagleInitMap k ei = do
  rePutStr (T.unpack (eagleName ei) <> " load") >> hFlush stdout
  e@EagleSub{..} <- eagleLoad ei{ eagleIngest = (eagleIngest ei){ ingestBlockSize = maxmap } }
  when (fromIntegral eagleLen >= maxmap) $ fail "exeeded maxmap"
  eagleTakeMap k e
    { eagleMap = IM.fromList $ zip (map fromIntegral $ VS.toList eagleKey) [0..]
    , eagleTake = eagleTakeMap
    }
  where
  maxmap = 100000000
  
eagleTakeMap :: Int32 -> EagleSub -> IO (Maybe DataValues, EagleSub)
eagleTakeMap k e = return (get <$> IM.lookup (fromIntegral k) (eagleMap e), e) where
  get i = map (second $ blockIndex i) (eagleBlock e)

eagleTake1 :: Int32 -> EagleSub -> IO (Maybe DataValues, EagleSub)
eagleTake1 k ei = do
  e@EagleSub{..} <- eagleLoad ei
  if ingestEOF eagleIngest then return (Nothing, e)
  else do
    let k1 = eagleKey VS.! eagleOff
    case compare k k1 of
      LT -> return (Nothing, e)
      EQ -> return (Just $ map (second $ blockIndex eagleOff) eagleBlock, e{ eagleOff = succ eagleOff })
      GT -> fail $ "eagleTake out of order " <> T.unpack eagleName <> " " <> show k <> " > " <> show k1

eagleTakeAll :: Int32 -> EagleSub -> IO ([DataValues], EagleSub)
eagleTakeAll k e = do
  (r, e') <- eagleTake1 k e
  maybe
    (return ([], e'))
    (\x -> first (x:) <$> eagleTakeAll k e')
    r

eagleAperture :: Int32 -> EagleSub -> IO (Maybe DataValues, EagleSub)
eagleAperture k e@EagleSub{} = do
  (r, e') <- eagleTakeAll k e
  return . (, e') $ if null r
    then Nothing
    else Just $ foldMap apv r
  where
  apv ((kn,Integer (Identity kv)):vs) | kn == eagleApertureField =
    map (first (<> ks)) vs
    where ks = T.pack ('_' : show kv)
  apv _ = error "eagleAperture"
eagleAperture k e@EagleBlock{ eagleBlocks = b } = do
  (v, b') <- unzip <$> mapM (\eb -> eagleTake eb k eb) b
  return (fold v, e{ eagleBlocks = b' })

eagleNext :: Int32 -> EagleSub -> IO (J.Series, EagleSub)
eagleNext k e = first (foldMap (blockJsonWith id)) <$> eagleTake e k e

ingestEagle :: Ingest -> M Word64
ingestEagle inginfo = do
  FieldValue Field{ fieldEnum = Just sime } (Byte (Identity sim)) <- constField "simulation" inginfo
  let simn = sime V.! fromIntegral sim
      ingfof b i l o = do
        liftIO $ rePutStr ("fof " ++ show o ++ "/" ++ show l) >> hFlush stdout
        let n = min (fromIntegral $ ingestBlockSize i) l
        ingestWith i (blockDoc i mempty b . (o +)) n
        when (l > n) $ ingfof b i (l - n) (o + n)
  liftBaseOp (withHDF5 $ ingestFile inginfo) $ \hf -> do
    info <- prepareIngest inginfo
      { ingestPrefix = show sim ++ "F"
      } hf
    (nfof, fof) <- liftBaseOp (withGroup hf (simn <> "_FoF")) $ \hg -> do
      liftIO $ rePutStr "fof load" >> hFlush stdout
      (n, b, _info') <- liftIO $ loadBlock info{ ingestBlockSize = maxfof, ingestOffset = 0 } hg
      when (fromIntegral n >= maxfof) $ fail "exeeded maxfof"
      ingfof b info n 0
      liftIO $ putStrLn ""
      return (n, b)
    nsub <- case ingestJoin info of
      Just IngestHaloJoin
        { joinIngest = subinfo@Ingest{ ingestJoin = Just (IngestJoin supfs) }
        , joinFirst = (`lookup` fof) -> Just (Long fofid)
        , joinParent = subgf
        } -> liftBaseOp (withGroup hf (simn <> "_Subhalo")) $ \hs -> do
        let fofmap = IM.fromDistinctAscList $ zip (map fromIntegral $ VS.toList fofid) [0..]
            loadsups [] sups = loop subinfo
              { ingestPrefix = show sim ++ "S"
              } sups
            loadsups (f:r) sups =
              liftBaseOp (withGroupMaybe hf (simn <> "_" <> fieldSource f)) $ \hg ->
                loadsups r $ maybe id ((:) . eagleNew simn (fieldSource f) subinfo
                  { ingestCatalog = (ingestCatalog subinfo){ catalogFieldGroups = fold (fieldSub f) }
                  , ingestOffset = 0
                  }) hg sups
            loop info sups = do
              liftIO $ rePutStr ("subhalo " <> show (ingestOffset info)) >> hFlush stdout
              (n, sb, info') <- liftIO $ loadBlock info hs
              gid <- case lookup subgf sb of
                Just (Long gid) -> return gid
                _ -> fail $ "Subhalo/" <> T.unpack subgf <> " not found"
              sid <- case lookup ("Subhalo_" <> eagleKeyField) sb of
                Just (Integer sid) -> return sid
                _ -> fail $ "Subhalo/" <> T.unpack eagleKeyField <> " not found"
              let doc s r i
                    | i == n = return (s, r)
                    | otherwise = do
                      (sd, s') <- unzip <$> mapM (eagleNext $ sid VS.! i) s
                      let d = blockDoc info
                            (foldMap (blockJson fof) (IM.lookup (fromIntegral $ gid VS.! i) fofmap) <> fold sd)
                            sb i
                      doc s' (d:r) (succ i)
              (sups', docs) <- liftIO $ doc sups [] 0
              ES.createBulk (ingestCatalog info') docs
              if ingestEOF info'
                then ingestOffset info' <$ liftIO (putStrLn "")
                else loop info' sups'
        loadsups (V.toList supfs) []
      _ -> fail "missing join"
    return (fromIntegral nfof+nsub)
  where
  maxfof = 100000000
