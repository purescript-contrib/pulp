module Pulp.System.FFI where

import Prelude

import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), EXCEPTION())
import Control.Monad.Eff.Console (CONSOLE())
import Data.Function.Uncurried
import Unsafe.Coerce (unsafeCoerce)

import Node.FS (FS())
import Node.ChildProcess (CHILD_PROCESS())
import Node.Buffer (BUFFER())
import Node.Process (PROCESS())
import Node.ReadLine (READLINE())
import Node.HTTP (HTTP())

foreign import data Node :: !
foreign import data NodeError :: *

type PulpEffects = (node :: Node, console :: CONSOLE, buffer :: BUFFER, fs :: FS, avar :: AVAR, err :: EXCEPTION, process :: PROCESS, cp :: CHILD_PROCESS, http :: HTTP, readline :: READLINE)

type EffN a = Eff PulpEffects a
type AffN a = Aff PulpEffects a

-- | A normal side-effecting node callback, taking 2 parameters: the first for
-- | an error, the second for success. The type of the success value should be
-- | the same as the type parameter.
foreign import data Callback :: * -> *

foreign import runNode'  :: forall a. Fn3 (Error -> EffN Unit) (a -> EffN Unit) (Callback a -> Unit) (EffN Unit)

runNode :: forall a. (Callback a -> Unit) -> AffN a
runNode fn = makeAff (\err win -> runFn3 runNode' err win fn)

-- | This is quite unsafe but often useful.
foreign import unsafeInspect :: forall a. a -> String

unsafeToEffN :: forall eff a. Eff eff a -> EffN a
unsafeToEffN = unsafeCoerce

unsafeToAffN :: forall eff a. Aff eff a -> AffN a
unsafeToAffN = unsafeCoerce
