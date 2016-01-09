module Pulp.Bower
  ( action
  , launchBower
  , printHelp
  ) where

import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Data.Maybe (Maybe(..))

import Pulp.Args
import Pulp.Exec (exec)
import Pulp.System.FFI
import Pulp.System.Which (which)
import Pulp.Outputter

action :: Action
action = Action \args -> do
  out <- getOutputter args
  out.err "[warn] `pulp dep` is deprecated. Please use `bower` directly instead."
  launchBower args.remainder
  out.err "[warn] Just in case you missed the earlier warning:"
  out.err "[warn] `pulp dep` is deprecated. Please use `bower` directly instead."

launchBower :: Array String -> AffN Unit
launchBower args = do
  executable <- attempt $ which "bower"
  either (const $ throwError $ error """No `bower` executable found.
Pulp no longer bundles Bower. You'll need to install it manually:

   $ npm install -g bower
""")
    (\e -> exec e args Nothing)
    executable

printHelp :: Outputter -> AffN Unit
printHelp out = do
  out.bolded "Dependency Management with Bower\n\n"
  out.write "The `pulp dep` command invokes the Bower package manager.\n"
  out.write "Run Bower commands like eg. `pulp dep install` instead of `bower install`.\n\n"
  out.write "Consult Bower's help page for the available commands:\n"

  launchBower (["--help"] ++ if out.monochrome then ["--no-color"] else [])
