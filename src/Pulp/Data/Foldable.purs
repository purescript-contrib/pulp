module Pulp.Data.Foldable where

import Prelude

import Data.Foldable
import Data.Maybe (Maybe(..))

import Pulp.Data.Monoid.Max

max :: forall f a. (Foldable f, Ord a) => f a -> Maybe a
max l = runMax $ foldMap (Max <<< Just) l
