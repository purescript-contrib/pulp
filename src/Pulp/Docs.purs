
module Pulp.Docs where

import Prelude

import Data.List (List(..), fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Version.Haskell (Version(..))
import Effect.Class (liftEffect)
import Node.Process as Process
import Pulp.Args (Action(..))
import Pulp.Args.Get (getFlag, getOption')
import Pulp.Exec (exec)
import Pulp.Files (defaultGlobs, sources, testGlobs)
import Pulp.Outputter (getOutputter)
import Pulp.Validate (getPursVersion)

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

  buildPathArgs <-
    if true --(pursVersion >= Version (fromFoldable [0,13,0]) Nil) do
      then do
        buildPath <- getOption' "buildPath" opts
        pure ["--compile-output", buildPath]
      else
        pure []

  exec "purs" (["docs"] <> buildPathArgs <> args.remainder <> sources globInputFiles) Nothing

  out.log "Documentation generated."
