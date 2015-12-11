module Pulp.System.TreeKill
  ( treeKill) where

import Prelude

import Pulp.System.FFI

foreign import treeKill :: Int -> String -> EffN Unit
