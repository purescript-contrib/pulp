module Pulp.System.TreeKill
  ( treeKill ) where

import Prelude
import Data.Posix (Pid())

import Pulp.System.FFI

foreign import treeKill :: Pid -> String -> EffN Unit
