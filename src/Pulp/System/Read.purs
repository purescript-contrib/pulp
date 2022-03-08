module Pulp.System.Read ( read ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Aff (Aff)
import Pulp.System.FFI (Callback, runNode)

type ReadOptions = { prompt :: String, silent :: Boolean }

foreign import readImpl :: Fn2 ReadOptions (Callback String) Unit

read :: ReadOptions -> Aff String
read opts = runNode $ runFn2 readImpl opts
