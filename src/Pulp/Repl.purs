module Pulp.Repl where

import Prelude
import Data.Maybe
import Data.Map as Map
import Data.Set as Set

import Pulp.Args
import Pulp.Exec
import Pulp.Files

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  globs <- Set.union <$> defaultGlobs opts
                     <*> testGlobs opts
  execInteractive "purs" (["repl"] <> sources globs <> args.remainder) Nothing
