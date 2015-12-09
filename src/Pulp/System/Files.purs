
module Pulp.System.Files
  ( mkdirIfNotExist
  , openTemp
  , createWriteStream
  , isENOENT
  , touch
  ) where

import Prelude
import Data.Function
import Data.Maybe
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class (catchJust)

import Node.FS (FileDescriptor())
import Node.FS.Aff (mkdir)

import Pulp.System.FFI
import Pulp.System.Stream

foreign import isEEXIST :: Error -> Boolean

mkdirIfNotExist :: forall e. String -> AffN Unit
mkdirIfNotExist dir =
  catchJust (\e -> if isEEXIST e then Just unit else Nothing)
            (mkdir dir)
            pure

type TempOptions = { prefix :: String, suffix :: String }
type TempFileInfo = { path :: String, fd :: FileDescriptor }

foreign import openTemp' :: Fn2 TempOptions (Callback TempFileInfo) Unit

openTemp :: forall e. TempOptions -> AffN TempFileInfo
openTemp opts = runNode $ runFn2 openTemp' opts

foreign import createWriteStream :: forall e. String -> EffN (NodeStream String)

foreign import isENOENT :: Error -> Boolean

foreign import touch' :: Fn2 String (Callback Unit) Unit

touch :: forall e. String -> AffN Unit
touch path = runNode $ runFn2 touch' path
