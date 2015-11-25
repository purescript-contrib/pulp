
module Pulp.System.Files
  ( mkdirIfNotExist
  ) where

import Prelude
import Data.Maybe
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class (catchJust)
import Pulp.System.FFI

import Node.FS.Aff (mkdir)

foreign import isEEXIST :: Error -> Boolean

mkdirIfNotExist :: forall e. String -> AffN e Unit
mkdirIfNotExist dir =
  catchJust (\e -> if isEEXIST e then Just unit else Nothing)
            (mkdir dir)
            pure
