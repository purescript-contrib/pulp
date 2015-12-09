module Pulp.System.Require where

import Prelude
import Pulp.System.FFI

foreign import unsafeRequire :: forall e a. String -> EffN a
foreign import requireResolve :: forall e. String -> EffN String
