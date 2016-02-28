module Pulp.System.Require where

import Pulp.System.FFI

foreign import unsafeRequire :: forall a. String -> EffN a
foreign import requireResolve :: String -> EffN String
