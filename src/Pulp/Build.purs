
module Pulp.Build
  ( action
  , testBuild
  ) where

import Prelude
import Data.Maybe
import Data.Map (union)
import Data.Set (unions, empty)
import Data.List (toList)
import Data.Traversable (sequence)
import Control.Monad.Eff.Class (liftEff)

import Pulp.System.FFI
import qualified Pulp.System.Process as Process
import qualified Pulp.System.Log as Log
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (psc)
import Pulp.Files

data BuildType = NormalBuild | TestBuild

instance eqBuildType :: Eq BuildType where
  eq NormalBuild NormalBuild = true
  eq TestBuild TestBuild     = true
  eq _ _                     = false

action :: Action
action = go NormalBuild

testBuild :: forall e. Args -> AffN e Unit
testBuild = runAction (go TestBuild)

go :: BuildType -> Action
go buildType = Action \args -> do
  let opts = union args.globalOpts args.commandOpts

  cwd <- liftEff Process.cwd
  Log.log $ "Building project in" ++ cwd
  globs <- unions <$> sequence (toList
                                  [ defaultGlobs opts
                                  , globsFromOption "includePaths" opts
                                  , if buildType == TestBuild
                                      then testGlobs opts
                                      else pure empty
                                  ])
  buildPath <- getOption' "buildPath" args.commandOpts

  psc (sources globs)
      (ffis globs)
      (["-o", buildPath] ++ args.remainder)
      Nothing
  Log.log "Build successful."
