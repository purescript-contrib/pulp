module Pulp.Data.Monoid.Max where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid

newtype Max a = Max (Maybe a)

runMax :: forall a. Max a -> Maybe a
runMax (Max m) = m

instance semigroupMax :: (Ord a) => Semigroup (Max a) where
  append a'@(Max (Just a)) b'@(Max (Just b)) = if a < b then b' else a'
  append a (Max Nothing) = a
  append (Max Nothing) b = b

instance monoidMax :: (Ord a) => Monoid (Max a) where
  mempty = Max Nothing
