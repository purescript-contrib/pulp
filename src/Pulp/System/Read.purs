module Pulp.System.Read ( read ) where

import Prelude

import Data.Function.Uncurried (Fn2(), runFn2)

import Pulp.System.FFI (AffN(), Callback(), runNode)

type ReadOptions = { prompt :: String, silent :: Boolean }

foreign import read' :: Fn2 ReadOptions (Callback String) Unit

read :: ReadOptions -> AffN String
read opts = runNode $ runFn2 read' opts
