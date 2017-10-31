
module Pulp.Sorcery (sorcery) where

import Prelude

import Control.Monad.Aff (makeAff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn3, mkEffFn1, runEffFn3)
import Pulp.System.FFI (EffN, PulpEffects, AffN)

foreign import sorceryImpl :: EffFn3 PulpEffects String (EffN Unit) (EffFn1 PulpEffects Error Unit) Unit

-- | Run sorcery given JS file
sorcery ::  String -> AffN Unit
sorcery file = makeAff \err succ -> runEffFn3 sorceryImpl file (succ unit) (mkEffFn1 err)