module Pulp.System.Files
       ( exists
       ) where

import Prelude

import Control.Monad.Aff (Aff())
import Data.Function

import Pulp.System.FFI

foreign import exists' :: Fn2 String (Callback Boolean) Unit

exists :: forall e. String -> AffN e Boolean
exists path = runNode $ runFn2 exists' path
