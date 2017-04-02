module Test.Main where

import Prelude
import Control.Monad.Eff (kind Effect, Eff, foreachE)
import Control.Monad.Eff.Console (log)

foreign import data PROCESS :: Effect

foreign import argv :: forall eff. Eff (process :: PROCESS | eff) (Array String)
foreign import slice :: forall a. Int -> Array a -> Array a
foreign import length :: forall a. Array a -> Int

drop :: forall a. Int -> Array a -> Array a
drop n arr
  | n < 1 = arr
  | n >= length arr = []
  | otherwise = slice n arr

main = do
    args <- drop 2 <$> argv
    foreachE args log
