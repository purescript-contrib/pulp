module Pulp.System.Stream where

import Data.Function

import Pulp.System.FFI

class Stream s a where
  write :: forall e. s a -> a -> AffN e Unit

foreign import data NodeStream :: * -> *

foreign import writeToNodeStream """
  function writeToNodeStream(stream, data, callback) {
    stream.write(data, callback);
  }""" :: forall a e. Fn3 (NodeStream a) a (Callback Unit) Unit

instance nodeStream :: Stream NodeStream a where
  write s a = runNode $ runFn3 writeToNodeStream s a
