
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
import Pulp.System.Ansi
import Pulp.System.Stream

action :: Action
action = Action \args -> launchBower args.remainder

launchBower :: forall e. Array String -> AffN e Unit
launchBower args = do
  bowerPath <- liftEff $ requireResolve "bower"
  let executable = Path.concat [bowerPath, "..", "..", "bin", "bower"]
  exec executable args Nothing

printHelp :: forall e. Ansi -> AffN e Unit
printHelp stream = do
  bolded stream "Dependency Management with Bower\n\n"
  write stream "The `pulp dep` command invokes the Bower package manager.\n"
  write stream "Run Bower commands like eg. `pulp dep install` instead of `bower install`.\n\n"
  write stream "Consult Bower's help page for the available commands:\n"

  launchBower ["--help"]

foreign import requireResolve :: forall e. String -> EffN e String
