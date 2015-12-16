module Pulp.System.SupportsColor
  ( hasBasic
  , has256
  ) where

import Prelude

foreign import supportLevel :: Int

hasBasic :: Boolean
hasBasic = supportLevel >= 1

has256 :: Boolean
has256 = supportLevel >= 2
