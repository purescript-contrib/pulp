
module Pulp.Version ( version ) where

import Prelude
import Data.Either (Either(..))
import Data.Version (Version(), parseVersion)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Node.FS.Sync as FS
import Node.Encoding (Encoding(UTF8))
import Node.Path as Path
import Data.Foreign (parseJSON)
import Data.Foreign.Class (readProp)

import Pulp.System.Process (__dirname)

version :: Version
version =
  case parseVersion versionString of
    Right v  -> v
    Left err -> unsafeThrow $ "pulp: Unable to parse version from package.json: "
                              ++ show err

versionString :: String
versionString =
  unsafePerformEff $ do
    json <- FS.readTextFile UTF8 (Path.concat [__dirname, "package.json"])
    case parseJSON json >>= readProp "version" of
      Left err ->
        throwException (error ("pulp: Unable to parse package.json: " ++ show err))
      Right v ->
        pure v
