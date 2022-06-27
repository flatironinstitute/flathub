module Monoid
  ( mwhen
  , mintersperse
  , mintersperseMap
  , joinMaybeWith
  , guarded
  ) where

import           Control.Applicative (Alternative, empty)

mwhen :: Monoid m => Bool -> m -> m
mwhen True v = v
mwhen False _ = mempty

mintersperse :: Monoid m => m -> [m] -> m
mintersperse _ [] = mempty
mintersperse d (x:l) = x <> foldMap (d <>) l

mintersperseMap :: Monoid m => m -> (a -> m) -> [a] -> m
mintersperseMap _ _ [] = mempty
mintersperseMap d f (x:l) = f x <> foldMap ((<>) d . f) l

joinMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
joinMaybeWith _ Nothing x = x
joinMaybeWith _ x Nothing = x
joinMaybeWith f (Just x) (Just y) = Just $ f x y

-- Not really Monoid but close enough
guarded :: Alternative m => Bool -> a -> m a
guarded True v = pure v
guarded False _ = empty
