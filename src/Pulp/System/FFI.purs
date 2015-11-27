module Pulp.System.FFI where

import Prelude

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Eff.Console (CONSOLE())
import Data.Function

import Node.FS (FS())
import Node.Buffer (BUFFER())

foreign import data Node :: !
foreign import data NodeError :: *

type PulpEffects e = (node :: Node, console :: CONSOLE, buffer :: BUFFER, fs :: FS, avar :: AVAR | e)
type EffN e a = Eff (PulpEffects e) a
type AffN e a = Aff (PulpEffects e) a

-- | A normal side-effecting node callback, taking 2 parameters: the first for
-- | an error, the second for success. The type of the success value should be
-- | the same as the type parameter.
foreign import data Callback :: * -> *

foreign import runNode'  :: forall a e. Fn3 (Error -> EffN e Unit) (a -> EffN e Unit) (Callback a -> Unit) (EffN e Unit)

runNode :: forall a e. (Callback a -> Unit) -> AffN e a
runNode fn = makeAff (\err win -> runFn3 runNode' err win fn)

-- | This is quite unsafe but often useful.
foreign import unsafeInspect :: forall a. a -> String
