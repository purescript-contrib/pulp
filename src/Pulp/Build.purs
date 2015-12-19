module Pulp.Build
  ( action
  , build
  , testBuild
  , getOutputStream
  ) where

import Prelude
import Control.Monad (when)
import Data.Maybe
import Data.Map (union)
import Data.String (split)
import Data.Set as Set
import Control.Monad.Eff.Class (liftEff)
import Node.Process as Process

import Pulp.System.FFI
import Pulp.System.Stream (write, WritableStream())
import Pulp.Outputter
import Pulp.System.Files as Files
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (psc, pscBundle)
import Pulp.Files
import Pulp.Rebuild as Rebuild

data BuildType = NormalBuild | TestBuild

instance eqBuildType :: Eq BuildType where
  eq NormalBuild NormalBuild = true
  eq TestBuild TestBuild     = true
  eq _ _                     = false

action :: Action
action = Action \args -> do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  srcPath        <- getOption' "srcPath" opts
  dependencyPath <- getOption' "dependencyPath" opts
  includePaths   <- fromMaybe [] <$> getOption "includePaths" opts
  let paths = [ srcPath, dependencyPath ] ++ includePaths

  needsRebuild <- Rebuild.needs args paths
  if needsRebuild
    then runAction (go NormalBuild) args
    else out.log "Project unchanged; skipping build step."

build :: Args -> AffN Unit
build = runAction action

testBuild :: Args -> AffN Unit
testBuild = runAction (go TestBuild)

go :: BuildType -> Action
go buildType = Action \args -> do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  cwd <- liftEff Process.cwd
  out.log $ "Building project in " ++ cwd

  globs <- Set.union <$> defaultGlobs opts
                     <*> (if buildType == TestBuild
                            then testGlobs opts
                            else pure Set.empty)

  buildPath <- getOption' "buildPath" args.commandOpts

  psc (sources globs)
      (ffis globs)
      (["-o", buildPath] ++ args.remainder)
      Nothing

  Rebuild.touch args

  out.log "Build successful."

  shouldBundle <- (||) <$> getFlag "optimise" opts <*> hasOption "to" opts
  when shouldBundle (bundle args)

bundle :: Args -> AffN Unit
bundle args = do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  out.log "Bundling JavaScript..."

  main      <- getOption' "main" opts
  modules   <- fromMaybe [] <<< map (split ",") <$> getOption "modules" opts
  buildPath <- getOption' "buildPath" opts

  bundledJs <- pscBundle (outputModules buildPath)
                         (["--module=" ++ main, "--main=" ++ main]
                           ++ map (\m -> "--module=" ++ m) modules
                           ++ args.remainder)
                          Nothing

  out' <- getOutputStream opts
  write out' bundledJs

  out.log "Bundled."

-- | Get a writable stream which output should be written to, based on the
-- | value of the 'to' option.
getOutputStream :: Options -> AffN WritableStream
getOutputStream opts = do
  to <- getOption "to" opts
  case to of
    Just path -> liftEff $ Files.createWriteStream path
    Nothing   -> pure Process.stdout
