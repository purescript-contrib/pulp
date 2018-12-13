
module Pulp.Version ( version, versionString, printVersion ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Version (Version, parseVersion, showVersion)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throwException, error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (readString)
import Foreign.Index (readProp)
import Foreign.JSON (parseJSON)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Globals (__dirname)
import Node.Path as Path
import Pulp.Exec (execQuiet)
import Pulp.System.Which (which)

version :: Version
version =
  case parseVersion versionString of
    Right v  -> v
    Left err -> unsafeThrow $ "pulp: Unable to parse version from package.json: "
                              <> show err

versionString :: String
versionString =
  unsafePerformEffect $ do
    json <- FS.readTextFile UTF8 (Path.concat [__dirname, "package.json"])
    case runExcept (parseJSON json >>= readProp "version" >>= readString) of
      Left err ->
        throwException (error ("pulp: Unable to parse package.json: " <> show err))
      Right v ->
        pure v

printVersion :: Aff Unit
printVersion = do
  pursVersion <- execQuiet "purs" ["--version"] Nothing
  pursPath <- attempt $ which "purs"
  liftEffect $ Console.log $
    "Pulp version " <> showVersion version <>
    "\npurs version " <> trim pursVersion <>
    either (const "") (\p -> " using " <> trim p) pursPath
