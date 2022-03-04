
module Pulp.System.Files
  ( mkdirIfNotExist
  , openTemp
  , tempDir
  , createWriteStream
  , isENOENT
  , touch
  ) where

import Prelude

import Control.Monad.Error.Class (catchJust)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Node.FS (FileDescriptor)
import Node.FS.Aff as FS
import Pulp.System.FFI (Callback, runNode)
import Pulp.System.Stream (WritableStream)

foreign import isEEXIST :: Error -> Boolean

mkdirIfNotExist :: String -> Aff Unit
mkdirIfNotExist dir =
  catchJust (\e -> if isEEXIST e then Just unit else Nothing)
            (FS.mkdir dir)
            pure

type TempOptions = { prefix :: String, suffix :: String }
type TempFileInfo = { path :: String, fd :: FileDescriptor }

foreign import openTempImpl :: Fn2 TempOptions (Callback TempFileInfo) Unit
foreign import tempDirImpl :: Fn2 TempOptions (Callback String) Unit

openTemp :: TempOptions -> Aff TempFileInfo
openTemp opts = runNode $ runFn2 openTempImpl opts

-- | Create a temporary directory and return its path.
tempDir :: TempOptions -> Aff String
tempDir opts = runNode $ runFn2 tempDirImpl opts

foreign import createWriteStream :: String -> Effect WritableStream

foreign import isENOENT :: Error -> Boolean

foreign import touchImpl :: Fn2 String (Callback Unit) Unit

touch :: String -> Aff Unit
touch path = runNode $ runFn2 touchImpl path
