module Test.Main where

import Prelude
import Effect (Effect, foreachE)
import Effect.Console (log)

foreign import argv :: Effect (Array String)
foreign import slice :: forall a. Int -> Array a -> Array a
foreign import length :: forall a. Array a -> Int

drop :: forall a. Int -> Array a -> Array a
drop n arr
  | n < 1 = arr
  | n >= length arr = []
  | otherwise = slice n arr

main :: Effect Unit
main = do
    args <- drop 2 <$> argv
    foreachE args log
