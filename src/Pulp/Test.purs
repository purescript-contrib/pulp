
module Pulp.Test
  ( action
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign (unsafeToForeign)
import Pulp.Args (Action(..), Options)
import Pulp.Args.Get (getOption')
import Pulp.Build as Build
import Pulp.Exec (exec)
import Pulp.Outputter (getOutputter)
import Pulp.Run (getNodeFlags, makeRunnableScript, setupEnv)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  runtime <- getOption' "runtime" opts
  let isNode = runtime == "node"
  let changeOpts = if isNode
                     then identity :: Options -> Options -- helps type inference
                     else Map.insert "to" (Just (unsafeToForeign "./output/test.js"))

  let buildArgs = args { remainder = []
                       , commandOpts = changeOpts args.commandOpts
                       }
  Build.testBuild buildArgs

  out.log "Running tests..."
  if isNode
    then do
      main <- getOption' "main" opts
      buildPath <- getOption' "buildPath" opts
      env <- setupEnv buildPath
      scriptFilePath <- makeRunnableScript { out, buildPath, prefix: "pulp-test", moduleName: main }
      nodeFlags <- getNodeFlags out runtime
      exec runtime
           (nodeFlags <> [scriptFilePath] <> args.remainder)
           (Just env)
    else do
      to <- getOption' "to" buildArgs.commandOpts
      exec runtime ([to] <> args.remainder) Nothing

  out.log "Tests OK."
