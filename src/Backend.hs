{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend
  ( FieldFilter(..)
  , Filters(..)
  , FieldStats(..)
  , StatsArgs(..)
  , queryStats
  , DataArgs(..)
  , queryData
  , maxResultWindow
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import           Data.Bits (xor)
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy)
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import           Data.Word (Word16)

import Monoid
import Type
import Field
import Catalog
import Global
import JSON
import ES
import qualified KeyedMap as KM

type Count = Word

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

data Filters = Filters
  { filterSample :: Double
  , filterSeed :: Maybe Word
  , filterFields :: KM.KeyedMap (FieldSub FieldFilter Proxy)
  }

instance Semigroup Filters where
  a <> b = Filters
    { filterSample = filterSample a *     filterSample b
    , filterSeed   = joinMaybeWith xor (filterSeed a) (filterSeed a)
    , filterFields = filterFields a <>    filterFields b
    }

instance Monoid Filters where
  mempty = Filters 1 Nothing mempty

filterQuery :: Filters -> J.Series
filterQuery Filters{..} = "query" .=*
  (if filterSample < 1
    then \q -> ("function_score" .=* ("query" .=* q
      <> "random_score" .=* foldMap (\s -> "seed" J..= s <> "field" J..= ("_seq_no" :: String)) filterSeed
      <> "boost_mode" J..= ("replace" :: String)
      <> "min_score" J..= (1 - filterSample)))
    else id)
  ("bool" .=* ("filter" `JE.pair` JE.list
    (\f -> JE.pairs $ unTypeValue (term f) $ fieldType f)
    (KM.toList filterFields)))
  where
  term f (FieldEQ [v]) = "term" .=* (fieldName f J..= v)
  term f (FieldEQ v@(_:_)) = "terms" .=* (fieldName f J..= v)
  term f (FieldRange g l) | typeIsNumeric (fieldType f) && all (\g' -> all (g' <=) l) g = "range" .=* (fieldName f .=* (bound "gte" g <> bound "lte" l))
    where bound t = foldMap (t J..=)
  term f (FieldWildcard w) | fieldWildcard f = "wildcard" .=* (fieldName f J..= w)
  term _ _ = error "invalid FieldFilder"

data FieldStats
  = FieldStats{ statsMin, statsMax, statsAvg :: Maybe Scientific, statsCount :: Count }
  | FieldTerms{ termsBuckets :: [(J.Value, Count)], termsCount :: Count }

fieldUseTerms :: Field -> Bool
fieldUseTerms f = fieldTerms f || not (typeIsNumeric (fieldType f))

parseStats :: Catalog -> J.Value -> J.Parser (Count, [(Field, FieldStats)])
parseStats cat = J.withObject "stats res" $ \o -> (,)
  <$> (o J..: "hits" >>= (J..: "total") >>= (J..: "value"))
  <*> (mapM pf . HM.toList =<< o J..: "aggregations") where
  pf (n, a) = do
    f <- lookupField cat n
    (,) f <$> (if fieldUseTerms f then pt else ps) a
  ps = J.withObject "stats" $ \o -> FieldStats
    <$> o J..: "min"
    <*> o J..: "max"
    <*> o J..: "avg"
    <*> o J..: "count"
  pt = J.withObject "terms" $ \o -> FieldTerms
    <$> (mapM pb =<< o J..: "buckets")
    <*> o J..: "sum_other_doc_count"
  pb = J.withObject "bucket" $ \o -> (,)
    <$> o J..: "key"
    <*> o J..: "doc_count"

data StatsArgs = StatsArgs
  { statsFilters :: Filters
  , statsFields :: KM.KeyedMap Field
  }

queryStats :: Catalog -> StatsArgs -> M (Count, [(Field, FieldStats)])
queryStats cat StatsArgs{..} =
  searchCatalog cat [] (parseStats cat) $ JE.pairs
    $  "track_total_hits" J..= True
    <> "size" J..= (0 :: Count)
    <> "aggs" .=* foldMap (\f -> fieldName f .=* (if fieldUseTerms f
      then "terms" .=* (field f <> "size" J..= (if fieldTerms f then 32 else 4 :: Int))
      else "stats" .=* field f)) statsFields
    <> filterQuery statsFilters
  where
  field = ("field" J..=) . fieldName

parseData :: Catalog -> J.Value -> J.Parser [KM.KeyedMap FieldValue]
parseData cat = J.withObject "data res" $ \o ->
  return []

data DataArgs = DataArgs
  { dataFilters :: Filters
  , dataFields :: KM.KeyedMap Field
  , dataSort :: [(Field, Bool)]
  , dataCount, dataOffset :: Word16
  }

queryData :: Catalog -> DataArgs -> M [KM.KeyedMap FieldValue]
queryData cat DataArgs{..} =
  searchCatalog cat [] (parseData cat) $ JE.pairs
    $  "size" J..= dataCount
    <> mwhen (dataOffset > 0) ("from" J..= dataOffset)
    <> "sort" `JE.pair` JE.list (\(f, a) -> JE.pairs (fieldName f J..= if a then "asc" else "desc" :: String)) (dataSort ++ [(def{ fieldName = "_doc" },True)])
    <> storedFieldSource (catalogStoreField (catalogStore cat)) J..= HM.keys dataFields
    <> filterQuery dataFilters
