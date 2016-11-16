module Pulp.Bower
  ( launchBower
  ) where

import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Data.Maybe (Maybe(..))

import Pulp.Exec (exec)
import Pulp.System.FFI
import Pulp.System.Which (which)

launchBower :: Array String -> AffN Unit
launchBower args = do
  executable <- attempt $ which "bower"
  either (const $ throwError $ error """No `bower` executable found.
Pulp no longer bundles Bower. You'll need to install it manually:

   $ npm install -g bower
""")
    (\e -> exec e args Nothing)
    executable
