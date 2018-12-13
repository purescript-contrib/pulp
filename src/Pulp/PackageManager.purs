module Pulp.PackageManager
  ( launchBower
  , launchPscPackage
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt)
import Effect.Exception (error)
import Pulp.Exec (exec)
import Pulp.System.Which (which)

run :: String -> String -> Array String -> Aff Unit
run execName errorMsg args = do
  executable <- attempt $ which execName
  either
    (const $ throwError $ error $ errorMsg')
    (\e -> exec e args Nothing)
    executable
  where errorMsg' = "No `" <> execName <> "` executable found.\n\n" <> errorMsg

launchBower :: Array String -> Aff Unit
launchBower = run "bower" """Pulp no longer bundles Bower. You'll need to install it manually:

   $ npm install -g bower
"""

launchPscPackage :: Array String -> Aff Unit
launchPscPackage = do
  run "psc-package" "Install psc-package from: https://github.com/purescript/psc-package"
