
module Pulp.System.Files
  ( mkdirIfNotExist
  , openTemp
  ) where

import Prelude
import Data.Function
import Data.Maybe
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class (catchJust)

import Node.FS (FileDescriptor())
import Node.FS.Aff (mkdir)

import Pulp.System.FFI

foreign import isEEXIST :: Error -> Boolean

mkdirIfNotExist :: forall e. String -> AffN e Unit
mkdirIfNotExist dir =
  catchJust (\e -> if isEEXIST e then Just unit else Nothing)
            (mkdir dir)
            pure

type TempOptions = { prefix :: String, suffix :: String }
type TempFileInfo = { path :: String, fd :: FileDescriptor }

foreign import openTemp' :: Fn2 TempOptions (Callback TempFileInfo) Unit

openTemp :: forall e. TempOptions -> AffN e TempFileInfo
openTemp opts = runNode $ runFn2 openTemp' opts
