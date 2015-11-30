
module Pulp.Watch where

import Prelude
import Data.Function
import Data.Maybe (fromMaybe)
import Data.Array as Array
import Data.Set as Set
import Data.Map as Map
import Data.Foldable (any, notElem)
import Control.Monad (when)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (runAff)

import Pulp.Args
import Pulp.Args.Get
import Pulp.Files
import Pulp.System.FFI
import Pulp.System.Log as Log
import Pulp.System.Process as Process
import Pulp.System.ChildProcess (ChildProcess(), fork, treeKill)

foreign import watch :: forall e.
  Array String
  -> (String -> EffN e Unit)
  -> EffN e Unit

foreign import minimatch :: String -> String -> Boolean

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts

  let argv' = Array.filter (`notElem` ["-w", "--watch"]) Process.argv
  child <- liftEff $ fork argv'

  srcPath        <- getOption' "srcPath" opts
  testPath       <- getOption' "testPath" opts
  dependencyPath <- getOption' "dependencyPath" opts
  includePaths   <- fromMaybe [] <$> getOption "includePaths" opts
  let directories = [ srcPath, testPath, dependencyPath ] ++ includePaths

  globs <- Set.union <$> defaultGlobs opts <*> testGlobs opts
  let fileGlobs = sources globs ++ ffis globs

  liftEff $ watch directories $ \path ->
    when (any (minimatch path) fileGlobs) do
      treeKill child.pid "SIGTERM"
      goAff $ Log.log "Source tree changed; restarting:"
      void $ liftEff $ fork argv'

  where
  goAff = runAff (const (pure unit)) (const (pure unit))
