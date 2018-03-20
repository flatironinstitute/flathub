module Monoid
  ( mwhen
  , mintersperse
  , mintersperseMap
  ) where

import           Data.Monoid ((<>))

mwhen :: Monoid m => Bool -> m -> m
mwhen True v = v
mwhen False _ = mempty

mintersperse :: Monoid m => m -> [m] -> m
mintersperse _ [] = mempty
mintersperse d (x:l) = x <> mconcat (map (d <>) l)

mintersperseMap :: Monoid m => m -> (a -> m) -> [a] -> m
mintersperseMap _ _ [] = mempty
mintersperseMap d f (x:l) = f x <> mconcat (map ((<>) d . f) l)
