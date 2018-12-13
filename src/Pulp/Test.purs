
module Pulp.Test
  ( action
  ) where

import Prelude
import Effect.Class (liftEffect)
import Data.Maybe
import Data.Map as Map
import Foreign (unsafeToForeign)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS

import Pulp.Outputter
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (exec)
import Pulp.Build as Build
import Pulp.Run (setupEnv, makeEntry)
import Pulp.System.Files (openTemp)

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
      noCheckMain <- getFlag "noCheckMain" opts
      when (not noCheckMain)
        (Build.checkEntryPoint out opts)

      main <- getOption' "main" opts
      buildPath <- getOption' "buildPath" opts
      env <- setupEnv buildPath
      info <- openTemp { prefix: "pulp-test", suffix: ".js" }
      src <- liftEffect $ Buffer.fromString (makeEntry main) UTF8
      _ <- FS.fdAppend info.fd src
      _ <- FS.fdClose info.fd
      exec runtime
           ([info.path] <> args.remainder)
           (Just env)
    else do
      to <- getOption' "to" buildArgs.commandOpts
      exec runtime ([to] <> args.remainder) Nothing

  out.log "Tests OK."
