module Pulp.System.Require where

import Effect (Effect)

foreign import unsafeRequire :: forall a. String -> Effect a
foreign import requireResolve :: String -> Effect String
