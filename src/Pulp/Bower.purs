
module Pulp.Bower
  ( action
  , launchBower
  , printHelp
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)
import Node.Path as Path

import Pulp.Args
import Pulp.Exec (exec)
import Pulp.System.FFI
import Pulp.System.Require (requireResolve)
import Pulp.Outputter

action :: Action
action = Action \args -> launchBower args.remainder

launchBower :: forall e. Array String -> AffN e Unit
launchBower args = do
  bowerPath <- liftEff $ requireResolve "bower"
  let executable = Path.concat [bowerPath, "..", "..", "bin", "bower"]
  exec "node" ([executable] ++ args) Nothing

printHelp :: forall e. Outputter e -> AffN e Unit
printHelp out = do
  out.bolded "Dependency Management with Bower\n\n"
  out.write "The `pulp dep` command invokes the Bower package manager.\n"
  out.write "Run Bower commands like eg. `pulp dep install` instead of `bower install`.\n\n"
  out.write "Consult Bower's help page for the available commands:\n"

  launchBower (["--help"] ++ if out.monochrome then ["--no-color"] else [])
