module Pulp.System.Ansi
       ( Ansi(..)
       , AnsiStream(..)
       , ansi
       , col
       , bgCol
       , bold
       , underline
       , reset
       ) where

import Prelude

import Data.Function

import Pulp.System.FFI
import Pulp.System.Stream

foreign import data AnsiStream :: * -> *

type Ansi = AnsiStream String

foreign import ansi :: NodeStream String -> Ansi

foreign import castToNodeStream :: Ansi -> NodeStream String

instance ansiStream :: Stream AnsiStream String where
  write s a = write (castToNodeStream s) a

foreign import col' :: Fn3 Ansi String (Callback Unit) Unit

col :: forall e. Ansi -> String -> AffN e Unit
col s c= runNode $ runFn3 col' s c

foreign import bgCol' :: Fn3 Ansi String (Callback Unit) Unit

bgCol :: forall e. Ansi -> String -> AffN e Unit
bgCol s c= runNode $ runFn3 bgCol' s c

foreign import bold' :: Fn2 Ansi (Callback Unit) Unit

bold :: forall e. Ansi -> AffN e Unit
bold s = runNode $ runFn2 bold' s

foreign import underline' :: Fn2 Ansi (Callback Unit) Unit

underline :: forall e. Ansi -> AffN e Unit
underline s = runNode $ runFn2 underline' s

foreign import reset' :: Fn2 Ansi (Callback Unit) Unit

reset :: forall e. Ansi -> AffN e Unit
reset s = runNode $ runFn2 reset' s
