
module Pulp.Test
  ( action
  ) where

import Prelude
import Control.Monad (when)
import Data.Maybe
import qualified Data.Map as Map
import Data.String (split)
import qualified Data.Set as Set
import Data.List (toList)
import Data.Foreign (toForeign)
import Data.Traversable (sequence)
import Control.Monad.Eff.Class (liftEff)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)

import Pulp.System.FFI
import qualified Pulp.System.Stream as Stream
import qualified Pulp.System.Process as Process
import qualified Pulp.System.Log as Log
import qualified Pulp.System.Files as Files
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (exec, psc, pscBundle)
import Pulp.Files
import qualified Pulp.Build as Build
import Pulp.Run (setupEnv)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts

  engine <- getOption' "engine" opts
  let isNode = engine == "node"
  let changeOpts = if isNode
                     then id :: Options -> Options -- helps type inference
                     else Map.insert "to" (Just (toForeign "./output/test.js"))

  let buildArgs = args { remainder = []
                       , commandOpts = changeOpts args.commandOpts
                       }
  Build.testBuild buildArgs

  Log.log "Running tests..."
  if isNode
    then do
      main <- getOption' "main" opts
      buildPath <- getOption' "buildPath" opts
      env <- setupEnv buildPath
      exec engine
           (["-e", "require('" ++ main ++ "').main()"] ++ args.remainder)
           (Just env)
    else do
      to <- getOption' "to" buildArgs.commandOpts
      exec engine ([to] ++ args.remainder) Nothing

  Log.log "Tests OK."
