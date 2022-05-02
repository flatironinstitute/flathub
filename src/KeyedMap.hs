{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module KeyedMap
  ( Keyed(..)
  , KeyedMap
  , HM.empty
  , singleton
  , HM.null
  , HM.size
  , member
  , lookup
  , insert
  , delete
  , toList
  , fromList
  ) where

import Prelude hiding (lookup)

import           Control.Arrow ((&&&))
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM

class Keyed a where
  type Key a
  key :: a -> Key a

type KeyedMap a = HM.HashMap (Key a) a

singleton :: (Keyed a, Hashable (Key a)) => a -> KeyedMap a
singleton x = HM.singleton (key x) x

member :: (Keyed a, Hashable (Key a), Eq (Key a)) => a -> KeyedMap a -> Bool
member = HM.member . key

lookup :: (Keyed a, Hashable (Key a), Eq (Key a)) => a -> KeyedMap a -> Maybe a
lookup = HM.lookup . key

insert :: (Keyed a, Hashable (Key a), Eq (Key a)) => a -> KeyedMap a -> KeyedMap a
insert x = HM.insert (key x) x

delete :: (Keyed a, Hashable (Key a), Eq (Key a)) => a -> KeyedMap a -> KeyedMap a
delete = HM.delete . key

toList :: KeyedMap a -> [a]
toList = HM.elems

fromList :: (Keyed a, Hashable (Key a), Eq (Key a)) => [a] -> KeyedMap a
fromList = HM.fromList . map (key &&& id)
