module Pulp.Data.Monoid.Max where

import Data.Foldable
import Data.Maybe (Maybe(..))
import Data.Monoid

newtype Max a = Max (Maybe a)

runMax :: forall a. Max a -> Maybe a
runMax (Max m) = m

instance semigroupMax :: (Ord a) => Semigroup (Max a) where
  (<>) a'@(Max (Just a)) b'@(Max (Just b)) = if a < b then b' else a'
  (<>) a (Max Nothing) = a
  (<>) (Max Nothing) b = b

instance monoidMax :: (Ord a) => Monoid (Max a) where
  mempty = Max Nothing
