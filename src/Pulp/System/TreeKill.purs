module Pulp.System.TreeKill
  ( treeKill ) where

import Prelude

import Data.Posix (Pid)
import Effect (Effect)

foreign import treeKill :: Pid -> String -> Effect Unit
