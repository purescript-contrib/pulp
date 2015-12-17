module Pulp.System.Stream
  ( ReadableStream()
  , WritableStream()
  , AnyStream()
  , forget
  , write
  ) where

import Prelude
import Control.Monad.Aff (makeAff)
import Node.Stream as Node
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

write :: WritableStream -> String -> AffN Unit
write stream str = makeAff (\_ done -> void (Node.writeString stream UTF8 str (done unit)))
