module Pulp.System.Which ( which ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Aff (Aff)
import Pulp.System.FFI (Callback, runNode)

foreign import whichImpl :: Fn2 String (Callback String) Unit

which :: String -> Aff String
which cmd = runNode $ runFn2 whichImpl cmd
