module Pulp.System.Stream
  ( ReadableStream()
  , WritableStream()
  , NodeStream()
  , forget
  , write
  ) where

import Prelude
import Control.Monad.Aff (makeAff)
import Node.Stream as Node
import Unsafe.Coerce (unsafeCoerce)

import Pulp.System.FFI

type ReadableStream a = Node.Readable () PulpEffects a
type WritableStream a = Node.Writable () PulpEffects a

-- | A stream which might or might not be readable or writable.
foreign import data NodeStream :: * -> *

-- | Forget about whether a particular stream is readable or writable.
forget :: forall eff r a. Node.Stream r eff a -> NodeStream a
forget = unsafeCoerce

write :: forall a. WritableStream a -> a -> AffN Unit
write stream chunk = makeAff (\_ done -> void (Node.write (workaround stream) chunk (done unit)))
  where
  -- temporary workaround for https://github.com/purescript-node/purescript-node-streams/issues/3
  workaround :: WritableStream a -> WritableStream String
  workaround = unsafeCoerce
