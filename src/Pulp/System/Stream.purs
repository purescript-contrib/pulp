module Pulp.System.Stream
  ( ReadableStream()
  , WritableStream()
  , AnyStream()
  , end
  , forget
  , write
  , concatStream
  , concatStreamToBuffer
  , createGzip
  ) where

import Prelude
import Data.Function (runFn2, Fn2)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (makeAff)
import Node.Stream as Node
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Unsafe.Coerce (unsafeCoerce)

import Pulp.System.FFI

type ReadableStream = Node.Readable () PulpEffects
type WritableStream = Node.Writable () PulpEffects

-- | A stream which might or might not be readable or writable.
foreign import data AnyStream :: *

-- | Forget about whether a particular stream is readable or writable.
forget :: forall eff r. Node.Stream r eff -> AnyStream
forget = unsafeCoerce

write :: forall r. Node.Writable r PulpEffects -> String -> AffN Unit
write stream str = makeAff (\_ done -> void (Node.writeString stream UTF8 str (done unit)))

end :: forall r. Node.Writable r PulpEffects -> AffN Unit
end stream = makeAff (\_ done -> void (Node.end stream (done unit)))

concatStream :: forall w. Node.Readable w PulpEffects -> AffN String
concatStream stream = do
  buf <- concatStreamToBuffer stream
  liftEff (Buffer.toString UTF8 buf)

concatStreamToBuffer :: forall w. Node.Readable w PulpEffects -> AffN Buffer
concatStreamToBuffer stream = runNode $ runFn2 concatStreamToBuffer' stream

foreign import concatStreamToBuffer' :: forall w. Fn2 (Node.Readable w PulpEffects) (Callback Buffer) Unit

foreign import createGzip :: EffN (Node.Duplex PulpEffects)
