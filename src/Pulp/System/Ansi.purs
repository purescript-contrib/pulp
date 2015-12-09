module Pulp.System.Ansi
       ( Ansi(..)
       , AnsiStream(..)
       , ansi
       , col
       , bgCol
       , bold
       , underline
       , reset
       , bolded
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

col :: Ansi -> String -> AffN Unit
col s c= runNode $ runFn3 col' s c

foreign import bgCol' :: Fn3 Ansi String (Callback Unit) Unit

bgCol :: Ansi -> String -> AffN Unit
bgCol s c= runNode $ runFn3 bgCol' s c

foreign import bold' :: Fn2 Ansi (Callback Unit) Unit

bold :: Ansi -> AffN Unit
bold s = runNode $ runFn2 bold' s

foreign import underline' :: Fn2 Ansi (Callback Unit) Unit

underline :: Ansi -> AffN Unit
underline s = runNode $ runFn2 underline' s

foreign import reset' :: Fn2 Ansi (Callback Unit) Unit

reset :: Ansi -> AffN Unit
reset s = runNode $ runFn2 reset' s

bolded :: Ansi -> String -> AffN Unit
bolded stream str = do
  bold stream
  write stream str
  reset stream
