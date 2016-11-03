module Pulp.Watch
  ( watch
  , watchAff
  , action
  , minimatch
  ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Array as Array
import Data.Set as Set
import Data.Map as Map
import Data.Foldable (any, notElem)
import Control.Monad (when)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Aff (Aff, runAff, launchAff, Canceler)
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

import Unsafe.Coerce

foreign import watch ::
  Array String
  -> (String -> EffN Unit)
  -> EffN Unit

removeErrLabel :: forall e a. Aff (err :: EXCEPTION | e) a -> Aff e a
removeErrLabel = unsafeCoerce

watchAff ::  Array String -> (String -> AffN Unit) -> AffN Unit
watchAff dirs callback =
  liftEff $ watch dirs $ \path -> void $ launchAff (removeErrLabel $ callback path)

foreign import minimatch :: String -> String -> Boolean

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  argv' <- liftEff $ Array.filter (_ `notElem` ["-w", "--watch"]) <<< Array.drop 2 <$> Process.argv
  childV <- AVar.makeVar
  liftEff (fork __filename argv') >>= AVar.putVar childV

  srcPath        <- getOption' "srcPath" opts
  testPath       <- getOption' "testPath" opts
  dependencyPath <- getOption' "dependencyPath" opts
  includePaths   <- fromMaybe [] <$> getOption "includePaths" opts
  let directories = [ srcPath, testPath, dependencyPath ] <> includePaths

  globs <- Set.union <$> defaultGlobs opts <*> testGlobs opts
  let fileGlobs = sources globs <> ffis globs

  watchAff directories $ \path -> do
    when (any (minimatch path) fileGlobs) do
      child <- AVar.takeVar childV
      liftEff $ treeKill (pid child) "SIGTERM"
      out.log "Source tree changed; restarting:"
      liftEff (fork __filename argv') >>= AVar.putVar childV
