{-# LANGUAGE OverloadedStrings #-}

module Backend
  ( FieldFilter(..)
  , Filters(..)
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import           Data.Bits (xor)
import           Data.Proxy (Proxy)

import Monoid
import Type
import Field
import Catalog
import Global
import JSON
import ES

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
  , filterFields :: [FieldSub FieldFilter Proxy]
  }

instance Semigroup Filters where
  a <> b = Filters
    { filterSample = filterSample a *     filterSample b
    , filterSeed   = joinMaybeWith xor (filterSeed a) (filterSeed a)
    , filterFields = filterFields a <>    filterFields b
    }

instance Monoid Filters where
  mempty = Filters 1 Nothing []

data FieldStats a
  = FieldStats{ statsMin, statsMax, statsAvg :: a, statsCount :: Count }
  | FieldBuckets{ buckets :: [(a, Count)], bucketsOther :: Count }

queryStats :: Catalog -> Filters -> [Field] -> M (Count, [FieldSub FieldStats Proxy])
queryStats cat filt fields = do
  () <- searchCatalog cat [] $ JE.pairs
    $  "track_total_hits" J..= True
    <> "aggs" .=* foldMap (\f -> fieldName f .=* (if fieldTerms f || not (typeIsNumeric (fieldType f))
      then "terms" .=* (field f <> "size" J..= (if fieldTerms f then 32 else 4 :: Int))
      else "stats" .=* field f)) fields
  fail "TODO"
  where
  field = ("field" J..=) . fieldName
