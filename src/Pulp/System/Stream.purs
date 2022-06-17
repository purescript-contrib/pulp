module Pulp.System.Stream
  ( AnyStream(..)
  , ReadableStream(..)
  , WritableStream(..)
  , concatStream
  , concatStreamToBuffer
  , createGzip
  , end
  , forget
  , stderr
  , stdout
  , streamFromString
  , write
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2, Fn2)
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.Process as Process
import Node.Stream as Node
import Pulp.System.FFI (Callback, runNode)
import Unsafe.Coerce (unsafeCoerce)

type ReadableStream = Node.Readable ()
type WritableStream = Node.Writable ()

-- | A stream which might or might not be readable or writable.
foreign import data AnyStream :: Type

-- | Forget about whether a particular stream is readable or writable.
forget :: forall r. Node.Stream r -> AnyStream
forget = unsafeCoerce

write :: forall r. Node.Writable r -> String -> Aff Unit
write stream str = makeAff (\cb -> mempty <* void (Node.writeString stream UTF8 str (cb (Right unit))))

end :: forall r. Node.Writable r -> Aff Unit
end stream = makeAff (\cb -> mempty <* void (Node.end stream (cb (Right unit))))

concatStream :: forall w. Node.Readable w -> Aff String
concatStream stream = do
  buf <- concatStreamToBuffer stream
  liftEffect (Buffer.toString UTF8 buf)

concatStreamToBuffer :: forall w. Node.Readable w -> Aff Buffer
concatStreamToBuffer stream = runNode $ runFn2 concatStreamToBufferImpl stream

foreign import concatStreamToBufferImpl :: forall w. Fn2 (Node.Readable w) (Callback Buffer) Unit

foreign import createGzip :: Effect (Node.Duplex)

stdout :: WritableStream
stdout = unsafeCoerce Process.stdout

stderr :: WritableStream
stderr = unsafeCoerce Process.stderr

foreign import streamFromString :: forall w. String -> Effect (Node.Readable w)
