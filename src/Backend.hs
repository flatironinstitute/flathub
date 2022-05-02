{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend
  ( FieldFilter(..)
  , Filters(..)
  , FieldStats(..)
  , StatsArgs(..)
  , queryStats
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import           Data.Bits (xor)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy)
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import           Data.Typeable (cast)

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

instance Functor FieldFilter where
  fmap f (FieldEQ l) = FieldEQ (map f l)
  fmap f (FieldRange g l) = FieldRange (fmap f g) (fmap f l)

instance Foldable FieldFilter where
  foldMap f (FieldEQ l) = foldMap f l
  foldMap f (FieldRange g l) = foldMap f g <> foldMap f l

instance Traversable FieldFilter where
  traverse f (FieldEQ l) = FieldEQ <$> traverse f l
  traverse f (FieldRange g l) = FieldRange <$> traverse f g <*> traverse f l

instance TypeTraversable FieldFilter where
  sequenceTypeValue (FieldEQ l) = fmapTypeValue FieldEQ (sequenceTypeValue l)
  sequenceTypeValue (FieldRange g l) = fmapTypeValue2 FieldRange (sequenceTypeValue g) (sequenceTypeValue l)

data Filters = Filters
  { filterSample :: Double
  , filterSeed :: Maybe Word
  , filterFields :: FieldMap FieldFilter Proxy
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
  term f (FieldEQ [v])
    | fieldWildcard f && any (T.any ('*' ==)) (cast v) = "wildcard" .=* (fieldName f J..= v)
    | otherwise = "term" .=* (fieldName f J..= v)
  term f (FieldEQ v) = "terms" .=* (fieldName f J..= v)
  term f (FieldRange g l) = "range" .=* (fieldName f .=* (bound "gte" g <> bound "lte" l))
    where bound t = foldMap (t J..=)

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
  , statsFields :: FieldMap Proxy Proxy
  }

instance Semigroup StatsArgs where
  a <> b = StatsArgs
    { statsFilters = statsFilters a <> statsFilters b
    , statsFields = statsFields a <> statsFields b
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
