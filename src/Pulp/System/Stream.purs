module Pulp.System.Stream where

import Prelude

import Data.Function

import Pulp.System.FFI

class Stream s a where
  write :: s a -> a -> AffN Unit

foreign import data NodeStream :: * -> *

foreign import writeToNodeStream :: forall a. Fn3 (NodeStream a) a (Callback Unit) Unit

instance nodeStream :: Stream NodeStream a where
  write s a = runNode $ runFn3 writeToNodeStream s a
