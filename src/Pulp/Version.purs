
module Pulp.Version ( version, printVersion ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.String (trim)
import Data.Version (Version(), parseVersion, showVersion)
import Control.Monad.Aff (makeAff, attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Node.FS.Sync as FS
import Node.Encoding (Encoding(UTF8))
import Node.ChildProcess as CP
import Node.Buffer as Buffer
import Node.Path as Path
import Data.Foreign (parseJSON)
import Data.Foreign.Class (readProp)
import Node.Globals (__dirname)

import Pulp.System.FFI (AffN())

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

printVersion :: AffN Unit
printVersion = do
  pscVersion <- exec' "psc --version"
  pscPath <- attempt $ exec' "which psc"
  liftEff $ Console.log $
    "Pulp version " ++ showVersion version ++
    "\npsc version " ++ trim pscVersion ++
    either (const "") (\p -> " using " ++ trim p) pscPath

  where
  exec' cmd =
    exec cmd CP.defaultExecOptions >>= \r ->
      liftEff (Buffer.toString UTF8 r.stdout)

exec :: String -> CP.ExecOptions -> AffN CP.ExecResult
exec cmd opts =
  makeAff \errback callback ->
    CP.exec cmd opts \r ->
      case r.error of
        Just err -> errback err
        Nothing -> callback r
