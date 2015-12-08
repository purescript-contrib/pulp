module Pulp.System.Require where

import Prelude
import Pulp.System.FFI

foreign import unsafeRequire :: forall e a. String -> EffN e a
foreign import requireResolve :: forall e. String -> EffN e String
