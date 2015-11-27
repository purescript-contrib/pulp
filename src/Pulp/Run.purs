
module Pulp.Run where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (toList, (:))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.StrMap (StrMap())
import qualified Data.StrMap as StrMap
import Data.String (replace)
import Data.Traversable (sequence)
import Control.Monad.Eff.Class (liftEff)
import qualified Node.Path as Path
import qualified Node.FS.Aff as FS
import qualified Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))

import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec
import Pulp.Files
import qualified Pulp.System.Process as Process
import qualified Pulp.System.Log as Log
import Pulp.System.Files (openTemp)
import Pulp.System.FFI

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts

  cwd <- liftEff Process.cwd
  Log.log $ "Building project in" ++ cwd

  globs <- defaultGlobs opts

  buildPath <- getOption' "buildPath" opts

  psc (sources globs)
      (ffis globs)
      ["-o", buildPath]
      Nothing
  Log.log "Build successful."

  main <- getOption' "main" opts
  src <- liftEff $ Buffer.fromString (makeEntry main) UTF8

  info <- openTemp { prefix: "pulp-run", suffix: ".js" }
  FS.fdAppend info.fd src
  FS.fdClose info.fd

  engine <- getOption' "engine" opts
  env <- setupEnv buildPath
  exec engine ([info.path] ++ args.remainder) (Just env)

-- | Given a build path, create an environment that is just like this process'
-- | environment, except with NODE_PATH set up for commands like `pulp run`.
setupEnv :: String -> forall e. AffN e (StrMap String)
setupEnv buildPath = do
  env <- liftEff Process.getEnvironment
  pure $ StrMap.alter (prependPath (Path.resolve [] buildPath))
                      "NODE_PATH"
                      env

prependPath :: String -> Maybe String -> Maybe String
prependPath newPath paths =
  Just $ case paths of
    Nothing -> newPath
    Just p  -> newPath ++ Path.delimiter ++ p

-- | Escape a string for insertion into a JS string literal.
jsEscape :: String -> String
jsEscape = replace "'" "\\'" <<< replace "\\" "\\\\"

-- | Construct a JS string to be used as an entry point from a module name.
makeEntry :: String -> String
makeEntry main = "require('" ++ jsEscape main ++ "').main();\n"
