module Pulp.Repl where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Pulp.Args (Action(..))
import Pulp.Exec (execInteractive)
import Pulp.Files (defaultGlobs, sources, testGlobs)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  globs <- Set.union <$> defaultGlobs opts
                     <*> testGlobs opts
  execInteractive "purs" (["repl"] <> sources globs <> args.remainder) Nothing
