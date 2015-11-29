
module Pulp.Bower
  ( bower
  , launchBower
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)
import Node.Path as Path

import Pulp.Args
import Pulp.Exec (exec)
import Pulp.System.FFI

bower :: Action
bower = Action \args -> launchBower args.remainder

launchBower :: forall e. Array String -> AffN e Unit
launchBower args = do
  bowerPath <- liftEff $ requireResolve "bower"
  let executable = Path.concat [bowerPath, "..", "..", "bin", "bower"]
  exec executable args Nothing

foreign import requireResolve :: forall e. String -> EffN e String
