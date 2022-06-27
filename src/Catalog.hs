{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Catalog
  ( Simulation
  , Catalog(..)
  , takeCatalogField
  , lookupField
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
  , QueryAgg(..)
  , Query(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow ((&&&))
import           Control.Monad (guard, unless, mfilter)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import           Data.Bits (xor)
import           Data.Functor.Classes (Show1(liftShowsPrec))
import           Data.Functor.Identity (Identity(runIdentity))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe (catMaybes, maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V

import Error
import Monoid
import Type
import Field
import JSON
import qualified KeyedMap as KM

-- |Just a catalog identifier (name)
type Simulation = T.Text

data Catalog = Catalog
  { catalogName :: !Simulation
  , catalogEnabled :: !Bool
  , catalogIndex :: !T.Text
  , catalogIndexSettings :: J.Object
  , catalogIngestPipeline :: Maybe T.Text
  , catalogVisible :: !Bool
  , catalogOrder :: !T.Text -- ^display order in catalog list
  , catalogTitle :: !T.Text
  , catalogSynopsis :: Maybe T.Text
  , catalogDescr :: Maybe T.Text
  , catalogHtml :: Maybe T.Text
  , catalogFieldGroups :: Fields
  , catalogFields :: Fields
  , catalogFieldMap :: KM.KeyedMap Field
  , catalogKey :: Maybe T.Text -- ^primary key
  , catalogSort :: [T.Text] -- ^sort field(s) for index
  , catalogCount :: Maybe Count -- ^(intended) number of rows
  }

instance KM.Keyed Catalog where
  type Key Catalog = Simulation
  key = catalogName

parseCatalog :: HM.HashMap T.Text Field -> T.Text -> J.Object -> J.Value -> J.Parser Catalog
parseCatalog dict catalogName stats = J.withObject "catalog" $ \c -> do
  catalogEnabled <- c J..:! "enabled" J..!= True
  catalogVisible <- c J..:! "visible" J..!= True
  catalogFieldGroups <- parseJSONField "fields" (J.withArray "fields" $ mapM (parseField dict stats)) c
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
  statsCount <- stats J..:? "count"
  catalogCount <- (<|> statsCount) <$> c J..:? "count"
  catalogIndex <- c J..:? "index" J..!= catalogName
  catalogIndexSettings <- c J..:? "settings" J..!= HM.empty
  catalogIngestPipeline <- c J..:? "pipeline"
  catalogOrder <- c J..:? "order" J..!= catalogName
  let catalogFields = expandFields catalogFieldGroups
      catalogFieldMap = KM.fromList $ V.toList catalogFields
  mapM_ (\k -> unless (HM.member k catalogFieldMap) $ fail "key field not found in catalog") catalogKey
  mapM_ (\k -> unless (HM.member k catalogFieldMap) $ fail "sort field not found in catalog") catalogSort
  return Catalog{..}

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
  , catalogFields      = V.filter ((n /=) . fieldName) $ catalogFields c
  , catalogFieldGroups = deleteField n               $ catalogFieldGroups c
  }) <$> HM.lookup n (catalogFieldMap c) where

lookupField :: MonadErr m => Catalog -> Bool -> T.Text -> m Field
lookupField _ _ "_id" = return idField
lookupField cat idx n =
  maybe (raise404 $ "Field not found: " <> show n) return
    $ mfilter (not . (&&) idx . or . fieldStore)
    $ HM.lookup n $ catalogFieldMap cat

data Grouping
  = GroupCatalog !T.Text
  | Grouping
    { groupName :: !T.Text
    , groupTitle :: !T.Text
    , groupHtml :: Maybe T.Text
    , groupings :: Groupings
    , groupVisible :: !Bool
    }
  deriving (Show)

instance KM.Keyed Grouping where
  type Key Grouping = T.Text
  key (GroupCatalog n) = n
  key Grouping{ groupName = n } = n

groupingName :: Grouping -> T.Text
groupingName (GroupCatalog n) = n
groupingName Grouping{ groupName = n } = n

groupingCatalog :: Catalogs -> Grouping -> Maybe Catalog
groupingCatalog cats (GroupCatalog c) = HM.lookup c $ catalogMap cats
groupingCatalog _ _ = Nothing

data Groupings = Groupings
  { groupList :: V.Vector Grouping
  , groupMap :: KM.KeyedMap Grouping
  } deriving (Show)

instance J.FromJSON Grouping where
  parseJSON (J.String c) = return $ GroupCatalog c
  parseJSON j = J.withObject "group" (\g -> do
    groupName <- g J..: "name"
    groupTitle <- g J..: "title"
    groupHtml <- g J..:? "html"
    groupings <- g J..: "children"
    groupVisible <- g J..:! "visible" J..!= True
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
  { catalogDict :: Fields
  , catalogMap :: !(KM.KeyedMap Catalog)
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
catalogGrouping Catalogs{ catalogGroupings = g } = Grouping "collections" "Collections" Nothing g True

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
    stats <- o J..:? "_stats" J..!= mempty
    cats <- HM.traverseWithKey (\n ->
        parseCatalog (expandAllFields dict) n (HM.lookupDefault mempty n stats))
      (HM.delete "_stats" $ HM.delete "_dict" $ HM.delete "_group" o)
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
    FilterRange (max la lb) (joinMaybeWith min ua ub)
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

instance Functor Filter where
  fmap f (FilterEQ l) = FilterEQ (f l)
  fmap f (FilterRange g l) = FilterRange (fmap f g) (fmap f l)

instance Foldable Filter where
  foldMap f (FilterEQ l) = f l
  foldMap f (FilterRange g l) = foldMap f g <> foldMap f l

instance Traversable Filter where
  traverse f (FilterEQ l) = FilterEQ <$> f l
  traverse f (FilterRange g l) = FilterRange <$> traverse f g <*> traverse f l

instance TypeTraversable Filter where
  sequenceTypeValue (FilterEQ v) =
    fmapTypeValue (FilterEQ . runIdentity) v
  sequenceTypeValue (FilterRange (Just l) (Just u)) =
    fmapTypeValue2 (\x y -> FilterRange (Just $ runIdentity x) (Just $ runIdentity y)) l u
  sequenceTypeValue (FilterRange (Just l) Nothing) =
     fmapTypeValue (\x -> FilterRange (Just $ runIdentity x) Nothing) l
  sequenceTypeValue (FilterRange Nothing (Just u)) =
     fmapTypeValue (\x -> FilterRange Nothing (Just $ runIdentity x)) u
  sequenceTypeValue (FilterRange Nothing Nothing) =
     Void $ FilterRange Nothing Nothing

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
  , queryFilter :: [FieldTypeValue Filter]
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
      [ "field" J..= fieldName (fieldDesc f)
      , "value" J..= fieldValue f
      ] | f <- queryFilter ]
    , "seed"   J..= querySeed
    , "sample" J..= querySample
    , "aggs"   J..= map (fieldName . queryAggField) queryAggs
    ]
