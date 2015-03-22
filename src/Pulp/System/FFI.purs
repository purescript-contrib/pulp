module Pulp.System.FFI where

import Control.Monad.Aff (Aff(..), Async(..), makeAff)
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Exception (Error())
import Data.Function

foreign import data Node :: !
foreign import data NodeError :: *

type EffN e a = Eff (node :: Node | e) a
type AffN e a = Aff (node :: Node | e) a

type Callback a = (NodeError -> a -> Unit) -> Unit

foreign import runNode' """
  function runNode$prime(error, success, fn) {
    return function() {
      fn(function(err, val) {
        if (err) { error(err)(); } else { success(val)(); }
      });
    };
  }""" :: forall a e. Fn3 (Error -> EffN e Unit) (a -> EffN e Unit) (Callback a -> Unit) (EffN (async :: Async | e) Unit)

runNode :: forall a e. (Callback a -> Unit) -> AffN e a
runNode fn = makeAff (\err win -> runFn3 runNode' err win fn)
