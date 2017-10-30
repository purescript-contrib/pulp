module Pulp.Clean
  ( action
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Pulp.Args (Action(..))
import Pulp.Outputter (Outputter, getOutputter)
import Pulp.System.FFI (AffN, Callback, runNode)


foreign import rimraf' :: Fn2 String (Callback Unit) Unit

rimraf :: String -> AffN Unit
rimraf glob = runNode $ runFn2 rimraf' glob

clean :: Outputter -> AffN Unit
clean out = do
  out.log $ "Cleaning project"
  rimraf "./output/"
  rimraf "./.pulp-cache/"

action :: Action
action = Action \args -> do
  out <- getOutputter args
  clean out
