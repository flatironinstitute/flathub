{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Backend
  ( FieldFilter(..)
  , Filters(..)
  , DataRow
  , DataArgs(..)
  , queryData
  , queryDataStream
  , maxDataCount, maxResultWindow
  , CountArgs
  , queryCount
  , StatsArgs(..)
  , queryStats
  , Histogram(..)
  , HistogramArgs(..)
  , maxHistogramSize, maxHistogramDepth
  , HistogramBucket(..)
  , HistogramResult(..)
  , queryHistogram
  ) where

import           Control.Applicative (many)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import           Data.Bits (xor)
import qualified Data.Conduit as C
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Reverse (Reverse(Reverse))
import qualified Data.HashMap.Strict as HM
import qualified Data.JsonStream.Parser as JS
import           Data.List (genericTake, nubBy)
import           Data.Maybe (fromMaybe, isJust, catMaybes, mapMaybe)
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word16)
import           Network.HTTP.Types.Method (StdMethod(GET))

import Error
import Monoid
import Type
import Field
import Catalog
import Global
import JSON
import ES
import qualified KeyedMap as KM

data FieldFilter a
  = FieldEQ [a]
  | FieldRange{ filterGTE, filterLTE :: Maybe a }
  | FieldWildcard{ filterWildcard :: T.Text }

instance Functor FieldFilter where
  fmap f (FieldEQ l) = FieldEQ (map f l)
  fmap f (FieldRange g l) = FieldRange (fmap f g) (fmap f l)
  fmap _ (FieldWildcard w) = FieldWildcard w

instance Foldable FieldFilter where
  foldMap f (FieldEQ l) = foldMap f l
  foldMap f (FieldRange g l) = foldMap f g <> foldMap f l
  foldMap _ (FieldWildcard _) = mempty

instance Traversable FieldFilter where
  traverse f (FieldEQ l) = FieldEQ <$> traverse f l
  traverse f (FieldRange g l) = FieldRange <$> traverse f g <*> traverse f l
  traverse _ (FieldWildcard w) = pure $ FieldWildcard w

instance TypeTraversable FieldFilter where
  sequenceTypeValue (FieldEQ l) = fmapTypeValue FieldEQ (sequenceTypeValue l)
  sequenceTypeValue (FieldRange g l) = fmapTypeValue2 FieldRange (sequenceTypeValue g) (sequenceTypeValue l)
  sequenceTypeValue (FieldWildcard w) = Void $ FieldWildcard w

instance J.ToJSON1 FieldFilter where
  liftToJSON tj _ (FieldEQ [x]) = tj x
  liftToJSON _ tjl (FieldEQ l) = tjl l
  liftToJSON tj _ (FieldRange g l) = J.object $ catMaybes
    [ (("gte" J..=) . tj) <$> g
    , (("lte" J..=) . tj) <$> l
    ]
  liftToJSON _ _ (FieldWildcard t) = J.object ["wildcard" J..= t]
  liftToEncoding tj _ (FieldEQ [x]) = tj x
  liftToEncoding _ tjl (FieldEQ l) = tjl l
  liftToEncoding tj _ (FieldRange g l) = J.pairs
    $  foldMap (JE.pair "gte" . tj) g
    <> foldMap (JE.pair "lte" . tj) l
  liftToEncoding _ _ (FieldWildcard t) = J.pairs $ "wildcard" J..= t

data Filters = Filters
  { filterSample :: Double
  , filterSeed :: Word
  , filterFields :: KM.KeyedMap (FieldTypeValue FieldFilter)
  }

instance Semigroup Filters where
  a <> b = Filters
    { filterSample = filterSample a *     filterSample b
    , filterSeed   = filterSeed   a `xor` filterSeed b
    , filterFields = filterFields a <>    filterFields b
    }

instance Monoid Filters where
  mempty = Filters 1 0 mempty

instance J.ToJSON Filters where
  toJSON Filters{..} = J.object $
    mwhen (filterSample < 1)
      [ "sample" J..= filterSample
      , "seed" J..= filterSeed
      ]
    ++
    map (\(FieldValue f v) -> JK.fromText (fieldName f) J..= v) (HM.elems filterFields)
  toEncoding Filters{..} = J.pairs $
    mwhen (filterSample < 1)
      ("sample" J..= filterSample
      <> "seed" J..= filterSeed)
    <>
    foldMap (\(FieldValue f v) -> JK.fromText (fieldName f) J..= v) filterFields

filterQuery :: Filters -> J.Series
filterQuery Filters{..} = "query" .=*
  (if filterSample < 1
    then \q -> ("function_score" .=* ("query" .=* q
      <> "random_score" .=* ("seed" J..= filterSeed <> "field" J..= J.String "_seq_no")
      <> "boost_mode" J..= ("replace" :: String)
      <> "min_score" J..= (1 - filterSample)))
    else id)
  ("bool" .=* ("filter" `JE.pair` JE.list
    (\(FieldValue f v) -> JE.pairs $ unTypeValue (term f) v)
    (KM.toList filterFields)))
  where
  term f (FieldEQ [v]) = "term" .=* (JK.fromText (fieldName f) J..= v)
  term f (FieldEQ v@(_:_)) = "terms" .=* (JK.fromText (fieldName f) J..= v)
  term f (FieldRange g l) | typeIsNumeric (fieldType f) && all (\g' -> all (g' <=) l) g = "range" .=* (JK.fromText (fieldName f ).=* (bound "gte" g <> bound "lte" l))
    where bound t = foldMap (t J..=)
  term f (FieldWildcard w) | fieldWildcard f = "wildcard" .=* (JK.fromText (fieldName f) J..= w)
  term _ _ = error "invalid FieldFilder"

class DataRow f r where
  parseHit :: f -> J.Object -> J.Parser r
  parseHitStream :: f -> JS.Parser r

instance DataRow a [(T.Text, J.Value)] where
  parseHit f = fmap HM.toList . parseHit f
  parseHitStream _ = (:)
    <$> "_id" JS..: ((,) "_id" <$> JS.value)
    <*> "fields" JS..: many (JS.objectItems JS.value)

instance DataRow a (HM.HashMap T.Text J.Value) where
  parseHit _ = return . JM.toHashMapText . storedFields
  parseHitStream = fmap HM.fromList . parseHitStream

instance DataRow (HM.HashMap T.Text Field) [FieldValue] where
  parseHit f = fmap KM.toList . parseHit f
  parseHitStream fields = (:)
    <$> "_id" JS..: pf idField
    <*> "fields" JS..: many (JS.objectKeyValues ps)
    where
    ps fn = foldMap pf $ HM.lookup fn fields
    pf f = makeFieldValueM f (Identity <$> parseStream)

parseFieldValue' :: Field -> J.Value -> Value
parseFieldValue' f v = fmapTypeValue (\Proxy -> either error Identity (J.parseEither parseJSONValue v)) (fieldType f)

instance DataRow (HM.HashMap T.Text Field) (HM.HashMap T.Text Value) where
  parseHit fm = return . HM.intersectionWith parseFieldValue' fm . JM.toHashMapText . storedFields
  parseHitStream fm = fmap (fieldValue :: FieldValue -> Value) <$> parseHitStream fm

instance DataRow (HM.HashMap T.Text Field) (HM.HashMap T.Text FieldValue) where
  parseHit fm = return . HM.intersectionWith pfv fm . JM.toHashMapText . storedFields where
    pfv f = setFieldValueUnsafe f . parseFieldValue' f
  parseHitStream fm = KM.fromList <$> parseHitStream fm

instance DataRow (V.Vector Field) (V.Vector (TypeValue Maybe)) where
  parseHit fields = getf . storedFields where
    getf o = mapM (parsef o) fields
    parsef o f = traverseTypeValue (\Proxy -> mapM parseJSONValue $ JM.lookup (JK.fromText $ fieldName f) o) (fieldType f)
  parseHitStream fields = (ev V.//)
    <$> many
      (  "_id" JS..: ps "_id"
      <> "fields" JS..: JS.objectKeyValues ps)
    where
    ps fn = foldMap pf $ HM.lookup fn fm
    pf (i, t) = (,) i <$> traverseTypeValue (\Proxy -> Just <$> parseStream) t
    ev = V.map (fmapTypeValue (\Proxy -> Nothing) . fieldType) fields
    fm = HM.fromList $ V.toList $ V.imap (\i f -> (fieldName f, (i, fieldType f))) fields

-------- data

parseData :: DataRow f a => f -> J.Value -> J.Parser (V.Vector a)
parseData fields = J.withObject "data res" $ \o ->
  o J..: "hits" >>= (J..: "hits") >>= mapM (parseHit fields)

type DataOffset = [J.Value]

parseDataStream :: DataRow f a => f -> JS.Parser (a, DataOffset)
parseDataStream fields = "hits" JS..: "hits" JS..: JS.arrayOf
  ((,)
   <$> parseHitStream fields
   <*> "sort" JS..: many (JS.arrayOf JS.value))

data DataArgs f = DataArgs
  { dataFilters :: Filters
  , dataFields :: f Field -- ^Which fields to return (in order)
  , dataSort :: [(Field, Bool)] -- ^Sort order: (field, true=asc/false=desc)
  , dataCount :: Word16 -- ^Number of rows to return
  , dataOffset :: Word16 -- ^Starting offset of rows to return
  }

maxDataCount :: Word16
maxDataCount = fromIntegral maxResultWindow

queryDataRequest :: (MonadGlobal m, Foldable f) => Catalog -> DataArgs f -> Maybe DataOffset -> m Request
queryDataRequest cat DataArgs{..} off =
  searchCatalogRequest cat [] $ JE.pairs
    $  "track_total_hits" J..= False
    <> "size" J..= dataCount
    <> "sort" `JE.pair` JE.list (\(f, a) ->
        JE.pairs (JK.fromText (fieldName f) J..= if a then "asc" else "desc" :: String))
      dataSort
    <> maybe
      (mwhen (dataOffset > 0) $ "from" J..= dataOffset)
      ("search_after" J..=) off
    <> storedFieldsArgs (toList dataFields)
    <> filterQuery dataFilters

-- |Query raw data rows as specified, and return a vector of row data, matching dataFields order.
queryData :: (Foldable f, DataRow (f Field) a) => Catalog -> DataArgs f -> M (V.Vector a)
queryData cat args@DataArgs{..} = do
  unless (dataCount <= maxDataCount && fromIntegral dataCount + fromIntegral dataOffset <= maxResultWindow)
    $ raise400 "count too large"
  httpJSON (parseData dataFields) =<< queryDataRequest cat args Nothing

fstAndLast :: Monad m => C.ConduitT (a, b) (C.Flush a) m (Maybe b)
fstAndLast = C.await >>= traverse go where
  loop b = C.await >>= maybe (return b) go
  go (a, b) = C.yield (C.Chunk a) >> loop b

queryDataStream :: (MonadResource m, MonadIO m, MonadGlobal m, MonadFail m, Foldable f, DataRow (f Field) a) =>
  Catalog -> DataArgs f -> C.ConduitT i (C.Flush a) m ()
queryDataStream cat args = qds Nothing where
  qds off = do
    req <- lift $ queryDataRequest cat args' off
    off' <- httpStream req
      C..| (mapM_ fail =<< parserConduit (parseDataStream (dataFields args')))
      C..| fstAndLast
    C.yield C.Flush
    mapM_ (qds . Just) off'
  args' = args{ dataSort = nubBy ((==) `on` fieldName . fst) $ dataSort args ++ map (, True) key }
  key = case mapMaybe (`HM.lookup` catalogFieldMap cat) $ catalogKey cat of
    [] -> [idField] -- XXX this is very bad for performance
    l -> l

-------- count

parseCount :: J.Value -> J.Parser Count
parseCount = J.withObject "count res" $ (J..: "count")

type CountArgs = Filters

queryCount :: Catalog -> CountArgs -> M Count
queryCount cat filt =
  httpJSON parseCount =<< elasticRequest GET (catalogURL cat ++ ["_count"]) []
    (JE.pairs $ filterQuery filt)

-------- stats

fieldUseTerms :: Field -> Bool
fieldUseTerms f = fieldTerms f || not (typeIsNumeric $ snd $ unArrayType $ fieldType f)

parseStats :: Catalog -> J.Value -> J.Parser (KM.KeyedMap (FieldTypeValue FieldStats))
parseStats cat = J.withObject "stats res" $ \o ->
  HM.traverseWithKey pf =<< o J..:! "aggregations" J..!= mempty
  where
  pf n a = do
    f <- failErr $ lookupField cat False n
    makeFieldValueM f $ (if fieldUseTerms f then pt else ps) a
  ps :: J.Value -> J.Parser (FieldStats a)
  ps = J.withObject "stats" $ \o -> FieldStats
    <$> o J..: "min"
    <*> o J..: "max"
    <*> o J..: "avg"
    <*> o J..: "count"
  pt :: Typed a => J.Value -> J.Parser (FieldStats a)
  pt = J.withObject "terms" $ \o -> FieldTerms
    <$> (mapM pb =<< o J..: "buckets")
    <*> o J..: "sum_other_doc_count"
  pb :: Typed a => J.Value -> J.Parser (a, Count)
  pb = J.withObject "bucket" $ \o -> (,)
    <$> (parseJSONValue =<< o J..: "key")
    <*> o J..: "doc_count"

data StatsArgs = StatsArgs
  { statsFilters :: Filters
  , statsFields :: KM.KeyedMap Field
  }

queryStats :: Catalog -> StatsArgs -> M (KM.KeyedMap (FieldTypeValue FieldStats))
queryStats cat StatsArgs{..} =
  searchCatalog cat [] (parseStats cat) $ JE.pairs
    $  "track_total_hits" J..= False
    <> "size" J..= (0 :: Count)
    <> "aggs" .=* foldMap (\f -> JK.fromText (fieldName f) .=* (if fieldUseTerms f
      then "terms" .=* (field f <> "size" J..= (if fieldTerms f then 32 else 4 :: Int))
      else "stats" .=* field f)) statsFields
    <> filterQuery statsFilters
  where
  field = ("field" J..=) . fieldName

-------- histogram

data Histogram = Histogram
  { histogramField :: Field
  , histogramSize :: Word16
  , histogramLog :: Bool
  }

data HistogramArgs = HistogramArgs
  { histogramFilters :: Filters
  , histogramFields :: [Histogram]
  , histogramQuartiles :: Maybe Field
  }

maxHistogramSize, maxHistogramDepth :: Word16
maxHistogramSize = 4096
maxHistogramDepth = 3

data HistogramInterval
  = HistogramInterval
    { histogramIntervalField :: Field
    , histogramInterval :: Scientific
    , histogramOffset :: Scientific
    }
  | HistogramRanges -- for log-scaled histograms
    { histogramIntervalField :: Field
    , histogramInterval :: Scientific -- ^really the ratio
    , histogramRanges :: [Double]
    }

data HistogramBucket = HistogramBucket
  { bucketKey :: Reverse [] Value
  , bucketCount :: Count
  , bucketQuartiles :: Maybe (TypeValue V.Vector)
  }

data HistogramResult = HistogramResult
  { histogramSizes :: [Scientific]
  , histogramBuckets :: [HistogramBucket]
  }

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = concat <$> mapM f l

parseHistogram :: HistogramArgs -> J.Value -> J.Parser [HistogramBucket]
parseHistogram HistogramArgs{..} = J.withObject "histogram res" $ \o ->
  o J..: "aggregations" >>= phist [] histogramFields where
  phist k (h:l) o =
    o J..: JK.fromText (fieldName (histogramField h)) >>= (J..: "buckets") >>= concatMapM pbuc where
    pbuc = J.withObject "hist bucket" $ \b -> do
      j <- b J..: (if histogramLog h then "from" else "key")
      v <- traverseTypeValue (\Proxy -> Identity <$> J.parseJSON j) (fieldType $ histogramField h)
      phist (v : k) l b
  phist k [] b = do
    c <- b J..: "doc_count"
    q <- mapM (pquart b) histogramQuartiles
    return [HistogramBucket (Reverse k) c q]
  pquart o f = do
    q <- o J..: JK.fromText (fieldName f) >>= (J..: "values") >>= mapM (J..: "value")
    traverseTypeValue (\Proxy -> mapM J.parseJSON q) (fieldType f)

scaleFromByTo :: Real a => a -> a -> a -> [a]
scaleFromByTo x r y
  | x < y = x : scaleFromByTo (x*r) r y
  | otherwise = [x]

remainder :: forall a . RealFrac a => a -> a -> a
remainder n d = d * snd (properFraction (n / d) :: (Integer, a))

queryHistogram :: Catalog -> HistogramArgs -> M HistogramResult
queryHistogram cat hist@HistogramArgs{..} = do
  unless (depth > 0)
    $ raise400 "empty histogram"
  unless (depth <= fromIntegral maxHistogramDepth
      && product (map (fromIntegral . histogramSize) histogramFields) <= maxHistogramSize)
    $ raise400 "histograms too large"
  -- calculate the bucket sizes (or explicit ranges for log)
  let size Histogram{..} = histint where
        histint
          | histogramLog = if typeIsFloating (fieldType histogramField) && fmin > 0 && fmax > fmin
            then let r = exp int in return $ HistogramRanges histogramField (fromFloatDigits r)
              $ genericTake (succ histogramSize) $ scaleFromByTo fmin r fmax
            else raise400 "invalid log histogram"
          | typeIsIntegral (fieldType histogramField) = return $
            let i = ceiling int in
            (HistogramInterval histogramField `on` fromInteger) i (floor fmin `mod` i)
          | otherwise = return $ (HistogramInterval histogramField `on` fromFloatDigits) int (remainder fmin int)
        int = if intd > 0 then intd else 1
        intd = (lmax - lmin) / fromIntegral histogramSize
        (lmin, lmax) = (logt fmin, logt fmax)
        logt = if histogramLog then log else id
        (fmin, fmax) = maybe stat (unTypeValue frng) $ look (filterFields histogramFilters)
        frng (FieldRange a b) = (maybe smin toDouble a, maybe smax toDouble b)
        frng _ = stat
        stat@(smin, smax) = fromMaybe (1, 10) $ unTypeValue srng =<< fieldStats histogramField
        look = fmap fieldValue . HM.lookup (fieldName histogramField)
  sizes <- runErr $ mapM size histogramFields
  dat <- searchCatalog cat [] (parseHistogram hist) $ JE.pairs
      $  "size" J..= (0 :: Count)
      <> filterQuery histogramFilters
      <> haggs sizes
  return $ HistogramResult (map histogramInterval sizes) dat
  where
  depth = length histogramFields + fromEnum (isJust histogramQuartiles)
  srng FieldStats{ statsMin = Just x, statsMax = Just y } = Just (toRealFloat x, toRealFloat y)
  srng _ = Nothing
  haggs (hi:l) =
    "aggs" .=* JK.fromText n .=* (hagg hi <> haggs l)
    where
    n = fieldName $ histogramIntervalField hi
    hagg HistogramInterval{ histogramInterval = i, histogramOffset = o } = "histogram" .=*
      (  "field" J..= n
      <> "interval" J..= i
      <> "offset" J..= o
      <> "min_doc_count" J..= J.Number 1)
    hagg HistogramRanges{ histogramRanges = r } = "range" .=*
      (  "field" J..= n
      <> "ranges" `JE.pair` JE.list id (range (0 :: Int) r))
  haggs [] = foldMap (\f -> "aggs" .=* JK.fromText (fieldName f) .=*
    "percentiles" .=* ("field" J..= fieldName f <> "percents" J..= [0::Int,25..100] <> "keyed" J..= False)) histogramQuartiles
  range i (x:r@(y:_)) = JE.pairs ("from" J..= x <> "to" J..= y <> "key" J..= i) : range (succ i) r
  range _ _ = []
