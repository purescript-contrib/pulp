
module Pulp.System.Files
  ( mkdirIfNotExist
  , openTemp
  , tempDir
  , createWriteStream
  , isENOENT
  , touch
  ) where

import Prelude
import Data.Function.Uncurried
import Data.Maybe
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class (catchJust)

import Node.FS (FileDescriptor)
import Node.FS.Aff as FS

import Pulp.System.FFI
import Pulp.System.Stream (WritableStream())

foreign import isEEXIST :: Error -> Boolean

mkdirIfNotExist :: String -> AffN Unit
mkdirIfNotExist dir =
  catchJust (\e -> if isEEXIST e then Just unit else Nothing)
            (FS.mkdir dir)
            pure

type TempOptions = { prefix :: String, suffix :: String }
type TempFileInfo = { path :: String, fd :: FileDescriptor }

foreign import openTemp' :: Fn2 TempOptions (Callback TempFileInfo) Unit
foreign import tempDir' :: Fn2 TempOptions (Callback String) Unit

openTemp :: TempOptions -> AffN TempFileInfo
openTemp opts = runNode $ runFn2 openTemp' opts

-- | Create a temporary directory and return its path.
tempDir :: TempOptions -> AffN String
tempDir opts = runNode $ runFn2 tempDir' opts

foreign import createWriteStream :: String -> EffN WritableStream

foreign import isENOENT :: Error -> Boolean

foreign import touch' :: Fn2 String (Callback Unit) Unit

touch :: String -> AffN Unit
touch path = runNode $ runFn2 touch' path
