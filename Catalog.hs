{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Catalog
  ( ESStoreField(..)
  , CatalogStore(..)
  , Catalog(..)
  , takeCatalogField
  , Catalogs(..)
  , Filter(..)
  , liftFilterValue
  , Query(..)
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad (unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import           Data.Bits (xor)
import           Data.Functor.Identity (Identity(runIdentity))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
import           Data.Proxy (Proxy(Proxy))
import           Data.Semigroup (Semigroup((<>)))
import qualified Data.Text as T

import Monoid
import Field
import JSON

data ESStoreField
  = ESStoreSource
  | ESStoreValues
  | ESStoreStore
  deriving (Eq, Ord, Enum, Show)

instance J.FromJSON ESStoreField where
  parseJSON J.Null                  = return ESStoreSource
  parseJSON (J.Bool False)          = return ESStoreValues
  parseJSON (J.Bool True)           = return ESStoreStore
  parseJSON (J.String "source")     = return ESStoreSource
  parseJSON (J.String "doc_values") = return ESStoreValues
  parseJSON (J.String "docvalue")   = return ESStoreValues
  parseJSON (J.String "value")      = return ESStoreValues
  parseJSON (J.String "store")      = return ESStoreStore
  parseJSON x = J.typeMismatch "ESStoreField" x

data CatalogStore
  = CatalogES
    { catalogIndex, catalogMapping :: !T.Text
    , catalogSettings :: J.Object
    , catalogStoreField :: !ESStoreField
    }

data Catalog = Catalog
  { catalogEnabled :: !Bool
  , catalogSort :: !T.Text
  , catalogTitle :: !T.Text
  , catalogDescr :: Maybe T.Text
  , catalogStore :: !CatalogStore
  , catalogFieldGroups :: FieldGroups
  , catalogFields :: Fields
  , catalogFieldMap :: HM.HashMap T.Text Field
  , catalogKey :: Maybe T.Text
  }

parseCatalog :: HM.HashMap T.Text FieldGroup -> J.Value -> J.Parser Catalog
parseCatalog dict = J.withObject "catalog" $ \c -> do
  catalogEnabled <- c J..:! "enabled" J..!= True
  catalogFieldGroups <- parseJSONField "fields" (J.withArray "fields" $ mapM (parseFieldGroup dict)) c
  catalogTitle <- c J..: "title"
  catalogSort <- c J..:? "sort" J..!= T.empty
  catalogDescr <- c J..:? "descr"
  catalogKey <- c J..:? "key"
  catalogStore <- CatalogES
      <$> (c J..: "index")
      <*> (c J..:! "mapping" J..!= "catalog")
      <*> (c J..:? "settings" J..!= HM.empty)
      <*> (c J..:? "store" J..!= ESStoreValues)
  let catalogFields = expandFields catalogFieldGroups
      catalogFieldMap = HM.fromList $ map (fieldName &&& id) catalogFields
  mapM_ (\k -> unless (HM.member k catalogFieldMap) $ fail "key field not found in catalog") catalogKey
  return Catalog{..}

instance J.FromJSON Catalog where
  parseJSON = parseCatalog mempty

instance J.ToJSON Catalog where
  toJSON Catalog{..} = J.object
    [ "title" J..= catalogTitle
    , "descr"  J..= catalogDescr
    , "fields" J..= catalogFields
    ] where

takeCatalogField :: T.Text -> Catalog -> Maybe (Field, Catalog)
takeCatalogField n c = (, c
  { catalogFieldMap    = HM.delete n                 $ catalogFieldMap c
  , catalogFields      = filter ((n /=) . fieldName) $ catalogFields c
  , catalogFieldGroups = deleteField n               $ catalogFieldGroups c
  }) <$> HM.lookup n (catalogFieldMap c) where

data Catalogs = Catalogs
  { catalogDict :: [Field]
  , catalogMap :: !(HM.HashMap T.Text Catalog)
  }

instance Semigroup Catalogs where
  a <> b = Catalogs
    { catalogDict = catalogDict a <> catalogDict b
    , catalogMap  = catalogMap  a <> catalogMap  b
    }

instance Monoid Catalogs where
  mempty = Catalogs mempty mempty
  mappend = (<>)

instance J.FromJSON Catalogs where
  parseJSON = J.withObject "top" $ \o -> do
    dict <- o J..:? "dict" J..!= mempty
    cats <- mapM (parseCatalog $ expandAllFields dict) (HM.delete "dict" o)
    return $ Catalogs (expandFields dict) cats

data Filter a
  = FilterEQ !a
  | FilterRange{ filterLB, filterUB :: Maybe a }

instance J.ToJSON1 Filter where
  liftToJSON f _ (FilterEQ x) = f x
  liftToJSON f _ (FilterRange l u) = J.object $ catMaybes $
    [("lb" J..=) . f <$> l, ("ub" J..=) . f <$> u]
  liftToEncoding f _ (FilterEQ x) = f x
  liftToEncoding f _ (FilterRange l u) = J.pairs $
    foldMap (JE.pair "lb" . f) l <> foldMap (JE.pair "ub" . f) u

liftFilterValue :: Field -> Filter Value -> FieldSub Filter Proxy
liftFilterValue f (FilterEQ v) =
  f{ fieldSub = Proxy, fieldType = fmapTypeValue (FilterEQ . runIdentity) v }
liftFilterValue f (FilterRange (Just l) (Just u)) =
  f{ fieldSub = Proxy, fieldType = fmapTypeValue2 (\x y -> FilterRange (Just $ runIdentity x) (Just $ runIdentity y)) l u }
liftFilterValue f (FilterRange (Just l) Nothing) =
  f{ fieldSub = Proxy, fieldType = fmapTypeValue (\x -> FilterRange (Just $ runIdentity x) Nothing) l }
liftFilterValue f (FilterRange Nothing (Just u)) =
  f{ fieldSub = Proxy, fieldType = fmapTypeValue (\x -> FilterRange Nothing (Just $ runIdentity x)) u }
liftFilterValue f (FilterRange Nothing Nothing) =
  f{ fieldSub = Proxy, fieldType = fmapTypeValue (\Proxy -> FilterRange Nothing Nothing) (fieldType f) }

data Query = Query
  { queryOffset :: Word
  , queryLimit :: Word
  , querySort :: [(Field, Bool)]
  , queryFields :: [Field]
  , queryFilter :: [(FieldSub Filter Proxy)]
  , querySample :: Double
  , querySeed :: Maybe Word
  , queryAggs :: [Field]
  , queryHist :: [(Field, Word)]
  }

instance Monoid Query where
  mempty = Query
    { queryOffset = 0
    , queryLimit  = 0
    , querySort   = []
    , queryFields = []
    , queryFilter = []
    , querySample = 1
    , querySeed   = Nothing
    , queryAggs   = []
    , queryHist   = []
    }
  mappend = (<>)

instance Semigroup Query where
  q1 <> q2 = Query
    { queryOffset = queryOffset q1 +     queryOffset q2
    , queryLimit  = queryLimit  q1 `max` queryLimit  q2
    , querySort   = querySort   q1 <>    querySort   q2
    , queryFields = queryFields q1 <>    queryFields q2
    , queryFilter = queryFilter q1 <>    queryFilter q2
    , querySample = querySample q1 *     querySample q2
    , querySeed   = joinMaybeWith xor (querySeed q1) (querySeed q2)
    , queryAggs   = queryAggs   q1 <>    queryAggs   q2
    , queryHist   = queryHist   q1 <>    queryHist q2
    }

instance J.ToJSON Query where
  toJSON Query{..} = J.object
    [ "offset" J..= queryOffset
    , "limit"  J..= queryLimit
    , "sort"   J..= [ J.object
      [ "field" J..= fieldName f
      , "asc" J..= a
      ] | (f,a) <- querySort ]
    , "fields" J..= map fieldName queryFields
    , "filter" J..= [ J.object
      [ "field" J..= fieldName f
      , "value" J..= unTypeValue J.toJSON1 (fieldType f)
      ] | f <- queryFilter ]
    , "seed"   J..= querySeed
    , "sample" J..= querySample
    , "aggs"   J..= map fieldName queryAggs
    , "hist"   J..= (fieldName . fst <$> queryHist)
    ]
