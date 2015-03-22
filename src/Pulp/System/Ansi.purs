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

import Data.Function

import Pulp.System.FFI
import Pulp.System.Stream

foreign import data AnsiStream :: * -> *

type Ansi = AnsiStream String

foreign import ansi """
  function ansi(stream) {
    return require("ansi")(stream);
  }""" :: NodeStream String -> Ansi

foreign import castToNodeStream """
  function castToNodeStream(stream) {
    return stream;
  }""" :: Ansi -> NodeStream String

instance ansiStream :: Stream AnsiStream String where
  write s a = write (castToNodeStream s) a

foreign import col' """
  function col$prime(stream, c, callback) {
    stream[c](); callback();
  }""" :: forall e. Fn3 Ansi String (Callback Unit) Unit

col :: forall e. Ansi -> String -> AffN e Unit
col s c= runNode $ runFn3 col' s c

foreign import bgCol' """
  function bgCol$prime(stream, c, callback) {
    stream.bg[c](); callback();
  }""" :: forall e. Fn3 Ansi String (Callback Unit) Unit

bgCol :: forall e. Ansi -> String -> AffN e Unit
bgCol s c= runNode $ runFn3 bgCol' s c

foreign import bold' """
  function bold$prime(stream, callback) {
    stream.bold(); callback();
  }""" :: forall e. Fn2 Ansi (Callback Unit) Unit

bold :: forall e. Ansi -> AffN e Unit
bold s = runNode $ runFn2 bold' s

foreign import underline' """
  function underline$prime(stream, callback) {
    stream.underline(); callback();
  }""" :: forall e. Fn2 Ansi (Callback Unit) Unit

underline :: forall e. Ansi -> AffN e Unit
underline s = runNode $ runFn2 underline' s

foreign import reset' """
  function reset$prime(stream, callback) {
    stream.reset(); callback();
  }""" :: forall e. Fn2 Ansi (Callback Unit) Unit

reset :: forall e. Ansi -> AffN e Unit
reset s = runNode $ runFn2 reset' s
