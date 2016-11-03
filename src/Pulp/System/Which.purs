module Pulp.System.Which ( which ) where

import Prelude

import Data.Function.Uncurried (Fn2(), runFn2)

import Pulp.System.FFI (AffN(), Callback(), runNode)

foreign import which' :: Fn2 String (Callback String) Unit

which :: String -> AffN String
which cmd = runNode $ runFn2 which' cmd
