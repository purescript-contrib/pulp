
module Pulp.Version ( version, versionString, printVersion ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.String (trim)
import Data.Version (Version(), parseVersion, showVersion)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Except (runExcept)
import Node.FS.Sync as FS
import Node.Encoding (Encoding(..))
import Node.Path as Path
import Data.Foreign (parseJSON)
import Data.Foreign.Class (readProp)
import Node.Globals (__dirname)

import Pulp.System.FFI (AffN)
import Pulp.System.Which (which)
import Pulp.Exec (execQuiet)

version :: Version
version =
  case parseVersion versionString of
    Right v  -> v
    Left err -> unsafeThrow $ "pulp: Unable to parse version from package.json: "
                              <> show err

versionString :: String
versionString =
  unsafePerformEff $ do
    json <- FS.readTextFile UTF8 (Path.concat [__dirname, "package.json"])
    case runExcept (parseJSON json >>= readProp "version") of
      Left err ->
        throwException (error ("pulp: Unable to parse package.json: " <> show err))
      Right v ->
        pure v

printVersion :: AffN Unit
printVersion = do
  pscVersion <- execQuiet "psc" ["--version"] Nothing
  pscPath <- attempt $ which "psc"
  liftEff $ Console.log $
    "Pulp version " <> showVersion version <>
    "\npsc version " <> trim pscVersion <>
    either (const "") (\p -> " using " <> trim p) pscPath
