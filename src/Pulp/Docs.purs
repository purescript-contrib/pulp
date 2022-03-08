
module Pulp.Docs where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Class (liftEffect)
import Node.Process as Process
import Pulp.Args (Action(..))
import Pulp.Args.Get (getFlag, getOption')
import Pulp.Exec (exec)
import Pulp.Files (defaultGlobs, sources, testGlobs)
import Pulp.Outputter (getOutputter)
import Pulp.Validate (dropPreRelBuildMeta, getPursVersion)
import Pulp.Versions.PureScript (psVersions)

action :: Action
action = Action \args -> do
  out <- getOutputter args
  pursVersion <- getPursVersion out

  cwd <- liftEffect Process.cwd
  out.log $ "Generating documentation in " <> cwd

  let opts = Map.union args.globalOpts args.commandOpts

  withTests <- getFlag "withTests" opts
  let includeWhen b act = if b then act else pure Set.empty
  globInputFiles <- Set.union <$> includeWhen withTests (testGlobs opts)
                              <*> defaultGlobs opts

  buildPath <- getOption' "buildPath" opts

  when ((dropPreRelBuildMeta pursVersion) < psVersions.v0_13_0)
    (out.log "Warning: 'pulp docs' now only supports 'purs' v0.13.0 and above. Please either update 'purs' or downgrade 'pulp'.")

  exec "purs" (["docs", "--compile-output", buildPath] <> args.remainder <> sources globInputFiles) Nothing

  out.log "Documentation generated."
