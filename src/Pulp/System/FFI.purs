module Pulp.System.FFI where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)

foreign import data NodeError :: Type

-- | A normal side-effecting node callback, taking 2 parameters: the first for
-- | an error, the second for success. The type of the success value should be
-- | the same as the type parameter.
foreign import data Callback :: Type -> Type

foreign import runNodeImpl  :: forall a. Fn3 (Error -> Effect Unit) (a -> Effect Unit) (Callback a -> Unit) (Effect Unit)

runNode :: forall a. (Callback a -> Unit) -> Aff a
runNode fn = makeAff (\cb -> mempty <* runFn3 runNodeImpl (cb <<< Left) (cb <<< Right) fn)

-- | This is quite unsafe but often useful.
foreign import unsafeInspect :: forall a. a -> String
