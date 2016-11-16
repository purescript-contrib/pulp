module Pulp.Watch
  ( watch
  , watchAff
  , watchDirectories
  , action
  , minimatch
  ) where

import Prelude
import Data.Maybe
import Data.Traversable (traverse, sequence)
import Data.Array as Array
import Data.Set as Set
import Data.Map as Map
import Data.Foldable (any, notElem)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.AVar as AVar
import Node.Process as Process
import Node.Globals (__filename)
import Node.ChildProcess (fork, pid)

import Pulp.Args
import Pulp.Args.Get
import Pulp.Files
import Pulp.System.FFI
import Pulp.System.TreeKill (treeKill)
import Pulp.Outputter
import Pulp.Utils

foreign import watch ::
  Array String
  -> (String -> EffN Unit)
  -> EffN Unit

watchAff ::  Array String -> (String -> AffN Unit) -> AffN Unit
watchAff dirs callback =
  liftEff $ watch dirs (void <<< launchAff <<< removeErrLabel <<< callback)

foreign import minimatch :: String -> String -> Boolean

-- Returns Nothing if the given Options did not include the relevant options
-- i.e. watching does not make sense with this command.
watchDirectories :: Options -> AffN (Maybe (Array String))
watchDirectories opts = do
  -- If any of these give Nothing, we shouldn't be using watchDirectories
  let basicPathOpts = ["srcPath", "testPath", "dependencyPath"]
  basicPaths <- traverse (flip getOption opts) basicPathOpts

  -- It's ok if this is Nothing, though.
  includePaths <- fromMaybe [] <$> getOption "includePaths" opts

  pure $ map (_ <> includePaths) (sequence basicPaths)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  -- It is important to do this before attempting to `fork` a separate process.
  directories <- watchDirectories opts >>= orErr "This command does not work with --watch"

  argv' <- liftEff $ Array.filter (_ `notElem` ["-w", "--watch"]) <<< Array.drop 2 <$> Process.argv
  childV <- AVar.makeVar
  liftEff (fork __filename argv') >>= AVar.putVar childV

  globs <- Set.union <$> defaultGlobs opts <*> testGlobs opts
  let fileGlobs = sources globs <> ffis globs

  watchAff directories $ \path -> do
    when (any (minimatch path) fileGlobs) do
      child <- AVar.takeVar childV
      liftEff $ treeKill (pid child) "SIGTERM"
      out.log "Source tree changed; restarting:"
      liftEff (fork __filename argv') >>= AVar.putVar childV
