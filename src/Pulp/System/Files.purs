module Pulp.System.Files
       ( exists
       ) where

import Control.Monad.Aff (Aff(..))
import Data.Function

import Pulp.System.FFI

foreign import exists' """
  function exists$prime(path, callback) {
    require("fs").exists(path, function(r) { callback(null, r); });
  }""" :: forall e. Fn2 String (Callback Boolean) Unit

exists :: forall e. String -> AffN e Boolean
exists path = runNode $ runFn2 exists' path
