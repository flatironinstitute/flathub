{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Catalog
  ( ESStoreField(..)
  , CatalogStore(..)
  , Catalog(..)
  , takeCatalogField
  , Grouping(..)
  , groupingName
  , groupingCatalog
  , Groupings(..)
  , lookupGrouping
  , findGroupsCatalog
  , Catalogs(..)
  , pruneCatalogs
  , catalogGrouping
  , groupedCatalogs
  , Filter(..)
  , liftFilterValue
  , QueryAgg(..)
  , Query(..)
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad (guard, unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import           Data.Bits (xor)
import           Data.Functor.Classes (Show1(liftShowsPrec))
import           Data.Functor.Identity (Identity(runIdentity))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.Vector as V

import Monoid
import Type
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
    { catalogIndex :: !T.Text
    , catalogSettings :: J.Object
    , catalogStoreField :: !ESStoreField
    }

data Catalog = Catalog
  { catalogEnabled :: !Bool
  , catalogVisible :: !Bool
  , catalogOrder :: !T.Text -- ^display order in catalog list
  , catalogTitle :: !T.Text
  , catalogSynopsis :: Maybe T.Text
  , catalogDescr :: Maybe T.Text
  , catalogHtml :: Maybe T.Text
  , catalogStore :: !CatalogStore
  , catalogFieldGroups :: FieldGroups
  , catalogFields :: Fields
  , catalogFieldMap :: HM.HashMap T.Text Field
  , catalogKey :: Maybe T.Text -- ^primary key (not really used)
  , catalogSort :: [T.Text] -- ^sort field(s) for index
  , catalogCount :: Maybe Word
  }

parseCatalog :: HM.HashMap T.Text FieldGroup -> J.Value -> J.Parser Catalog
parseCatalog dict = J.withObject "catalog" $ \c -> do
  catalogEnabled <- c J..:! "enabled" J..!= True
  catalogVisible <- c J..:! "visible" J..!= True
  catalogFieldGroups <- parseJSONField "fields" (J.withArray "fields" $ mapM (parseFieldGroup dict)) c
  catalogTitle <- c J..: "title"
  catalogSynopsis <- c J..:? "synopsis"
  catalogDescr <- c J..:? "descr"
  catalogHtml <- c J..:? "html"
  catalogKey <- c J..:? "key"
  catalogSort <- case HM.lookup "sort" c of
    Nothing -> return []
    Just J.Null -> return []
    Just (J.String s) -> return [s]
    Just s -> J.parseJSON s
  catalogCount <- c J..:? "count"
  catalogStore <- CatalogES
      <$> (c J..: "index")
      <*> (c J..:? "settings" J..!= HM.empty)
      <*> (c J..:? "store" J..!= ESStoreValues)
  catalogOrder <- c J..:? "order" J..!= catalogIndex catalogStore
  let catalogFields = expandFields catalogFieldGroups
      catalogFieldMap = HM.fromList $ map (fieldName &&& id) catalogFields
  mapM_ (\k -> unless (HM.member k catalogFieldMap) $ fail "key field not found in catalog") catalogKey
  mapM_ (\k -> unless (HM.member k catalogFieldMap) $ fail "sort field not found in catalog") catalogSort
  return Catalog{..}

instance J.FromJSON Catalog where
  parseJSON = parseCatalog mempty

instance J.ToJSON Catalog where
  toJSON Catalog{..} = J.object $
    [ "title" J..= catalogTitle
    , "synopsis"  J..= catalogSynopsis
    , "descr"  J..= catalogDescr
    , "fields" J..= catalogFields
    ] ++ concatMap maybeToList
    [ ("count" J..=) <$> catalogCount
    , case catalogSort of
      [] -> Nothing
      s -> Just $ "sort" J..= s
    ]

takeCatalogField :: T.Text -> Catalog -> Maybe (Field, Catalog)
takeCatalogField n c = (, c
  { catalogFieldMap    = HM.delete n                 $ catalogFieldMap c
  , catalogFields      = filter ((n /=) . fieldName) $ catalogFields c
  , catalogFieldGroups = deleteField n               $ catalogFieldGroups c
  }) <$> HM.lookup n (catalogFieldMap c) where

data Grouping
  = GroupCatalog !T.Text
  | Grouping
    { groupName :: !T.Text
    , groupTitle :: !T.Text
    , groupHtml :: Maybe T.Text
    , groupings :: Groupings
    }
  deriving (Show)

groupingName :: Grouping -> T.Text
groupingName (GroupCatalog n) = n
groupingName Grouping{ groupName = n } = n

groupingCatalog :: Catalogs -> Grouping -> Maybe Catalog
groupingCatalog cats (GroupCatalog c) = HM.lookup c $ catalogMap cats
groupingCatalog _ _ = Nothing

data Groupings = Groupings
  { groupList :: V.Vector Grouping
  , groupMap :: HM.HashMap T.Text Grouping
  } deriving (Show)

instance J.FromJSON Grouping where
  parseJSON (J.String c) = return $ GroupCatalog c
  parseJSON j = J.withObject "group" (\g -> do
    groupName <- g J..: "name"
    groupTitle <- g J..: "title"
    groupHtml <- g J..:? "html"
    groupings <- g J..: "children"
    return Grouping{..}) j

grouping :: V.Vector Grouping -> Groupings
grouping l = Groupings l $ HM.fromList $ map (groupingName &&& id) $ V.toList l

instance J.FromJSON Groupings where
  parseJSON j = grouping <$> J.parseJSON j

instance Semigroup Groupings where
  a <> b = Groupings
    { groupList = groupList a <> groupList b
    , groupMap = groupMap a <> groupMap b
    }

instance Monoid Groupings where
  mempty = Groupings mempty mempty
  mappend = (<>)

lookupGrouping :: [T.Text] -> Grouping -> Maybe Grouping
lookupGrouping [] g = Just g
lookupGrouping (h:l) Grouping{ groupings = Groupings{ groupMap = m } } =
  lookupGrouping l =<< HM.lookup h m
lookupGrouping _ _ = Nothing

groupingCatalogs :: Grouping -> HS.HashSet T.Text
groupingCatalogs (GroupCatalog t) = HS.singleton t
groupingCatalogs Grouping{ groupings = g } = groupingsCatalogs g

groupingsCatalogs :: Groupings -> HS.HashSet T.Text
groupingsCatalogs Groupings{ groupList = v } = foldMap groupingCatalogs v

findGroupCatalog :: T.Text -> Grouping -> [[T.Text]]
findGroupCatalog t (GroupCatalog s)
  | s == t = [[s]]
  | otherwise = []
findGroupCatalog t Grouping{ groupName = n, groupings = g } = map (n:) $ findGroupsCatalog t g

findGroupsCatalog :: T.Text -> Groupings -> [[T.Text]]
findGroupsCatalog t = foldMap (findGroupCatalog t) . groupList

data Catalogs = Catalogs
  { catalogDict :: [Field]
  , catalogMap :: !(HM.HashMap T.Text Catalog)
  , catalogGroupings :: Groupings
  }

pruneCatalogs :: HM.HashMap T.Text a -> Catalogs -> Catalogs
pruneCatalogs errs cats = cats
  { catalogMap = cm
  , catalogGroupings = pgs $ catalogGroupings cats
  }
  where
  cm = HM.filter catalogEnabled $ catalogMap cats `HM.difference` errs
  pg g@(GroupCatalog c) = g <$ guard (HM.member c cm)
  pg g = g{ groupings = g' } <$ guard (not $ V.null $ groupList g')
    where g' = pgs $ groupings g
  pgs g = grouping $ V.mapMaybe pg $ groupList g

-- |Virtual top-level grouping
catalogGrouping :: Catalogs -> Grouping
catalogGrouping Catalogs{ catalogGroupings = g } = Grouping "collections" "Collections" Nothing g

groupedCatalogs :: [T.Text] -> Catalogs -> Maybe Catalogs
groupedCatalogs [] c = Just c
groupedCatalogs p c = do
  g <- lookupGrouping p (catalogGrouping c)
  return c{ catalogMap = HM.intersection (catalogMap c) $ HS.toMap $ groupingCatalogs g }

instance Semigroup Catalogs where
  a <> b = Catalogs
    { catalogDict = catalogDict a <> catalogDict b
    , catalogMap  = catalogMap  a <> catalogMap  b
    , catalogGroupings = catalogGroupings a <> catalogGroupings b
    }

instance Monoid Catalogs where
  mempty = Catalogs mempty mempty mempty
  mappend = (<>)

instance J.FromJSON Catalogs where
  parseJSON = J.withObject "top" $ \o -> do
    dict <- o J..:? "_dict" J..!= mempty
    groups <- o J..:? "_group" J..!= mempty
    cats <- mapM (parseCatalog $ expandAllFields dict) (HM.delete "_dict" $ HM.delete "_group" o)
    mapM_ (\c -> unless (HM.member c cats) $ fail $ "Group catalog " ++ show c ++ " not found") $ groupingsCatalogs groups
    return $ Catalogs (expandFields dict) cats groups

data Filter a
  = FilterEQ a
  | FilterRange{ filterLB, filterUB :: Maybe a }
  deriving (Show)

instance Show1 Filter where
  liftShowsPrec sp _ p (FilterEQ x) = showParen (p > 10) $
    showString "FilterEQ " . sp 11 x
  liftShowsPrec sp lsp p (FilterRange l u) = showParen (p > 10) $
    showString "FilterRange " . liftShowsPrec sp lsp 11 l . showChar ' ' . liftShowsPrec sp lsp 11 u

-- |Intersection
instance Ord a => Semigroup (Filter a) where
  FilterEQ a <> FilterEQ b = case compare a b of
    EQ -> FilterEQ a
    LT -> FilterRange (Just b) (Just a)
    GT -> FilterRange (Just a) (Just b)
  FilterEQ a <> FilterRange l u
    | all (a <) l = FilterRange l (Just a)
    | all (a >) u = FilterRange (Just a) u
    | otherwise = FilterEQ a
  FilterRange la ua <> FilterRange lb ub =
    FilterRange (max la lb) (min ua ub)
  r <> e = e <> r

-- |'mempty' is unbounded
instance Ord a => Monoid (Filter a) where
  mempty = FilterRange Nothing Nothing
  mappend = (<>)

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

data QueryAgg
  = QueryStats
    { queryAggField :: Field
    }
  | QueryPercentiles
    { queryAggField :: Field
    , queryPercentiles :: [Float]
    }
  | QueryHist
    { queryAggField :: Field
    , queryHistSize :: Word
    , queryHistLog :: Bool
    , queryHistAggs :: [QueryAgg]
    }

data Query = Query
  { queryOffset :: Word
  , queryLimit :: Word
  , querySort :: [(Field, Bool)]
  , queryFields :: [Field]
  , queryFilter :: [FieldSub Filter Proxy]
  , querySample :: Double
  , querySeed :: Maybe Word
  , queryAggs :: [QueryAgg]
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
      , "value" J..= fieldType f
      ] | f <- queryFilter ]
    , "seed"   J..= querySeed
    , "sample" J..= querySample
    , "aggs"   J..= map (fieldName . queryAggField) queryAggs
    ]
