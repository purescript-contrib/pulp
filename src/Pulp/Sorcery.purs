
module Pulp.Sorcery (sorcery) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)

foreign import sorceryImpl :: EffectFn3 String (Effect Unit) (EffectFn1 Error Unit) Unit

-- | Run sorcery given JS file
sorcery ::  String -> Aff Unit
sorcery file = makeAff \cb -> mempty <* runEffectFn3 sorceryImpl file (cb (Right unit)) (mkEffectFn1 (cb <<< Left))
