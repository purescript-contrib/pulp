module Test.Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (log)
import Data.Array (drop)

foreign import data PROCESS :: !

foreign import argv :: forall eff. Eff (process :: PROCESS | eff) (Array String)

main = do
    args <- drop 2 <$> argv
    foreachE args log
