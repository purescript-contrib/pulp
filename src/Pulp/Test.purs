
module Pulp.Test
  ( action
  ) where

import Prelude
import Data.Maybe
import Data.Map as Map
import Data.Foreign (toForeign)

import Pulp.Outputter
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (exec)
import Pulp.Build as Build
import Pulp.Run (setupEnv)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  runtime <- getOption' "runtime" opts
  let isNode = runtime == "node"
  let changeOpts = if isNode
                     then id :: Options -> Options -- helps type inference
                     else Map.insert "to" (Just (toForeign "./output/test.js"))

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
      exec runtime
           (["-e", "require('" <> main <> "').main()"] <> args.remainder)
           (Just env)
    else do
      to <- getOption' "to" buildArgs.commandOpts
      exec runtime ([to] <> args.remainder) Nothing

  out.log "Tests OK."
