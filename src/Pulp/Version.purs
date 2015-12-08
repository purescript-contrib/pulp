
module Pulp.Version ( version ) where

import Prelude
import Data.Either (Either(..))
import Data.Version (Version(), parseVersion)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Pulp.System.Require (unsafeRequire)

version :: Version
version =
  case parseVersion versionString of
    Right v  -> v
    Left err -> unsafeThrow $ "pulp: Unable to parse version from package.json: "
                              ++ show err

versionString :: String
versionString = _.version (unsafePerformEff (unsafeRequire "./package.json"))
