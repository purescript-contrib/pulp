module Pulp.System.Process
       ( commandName
       , argv
       , exit
       , stdin
       , stdout
       , stderr
       ) where

import Control.Monad.Eff (Eff(..))

import Data.Array (drop, (!!))
import Data.Function
import Data.Maybe (fromMaybe)

import Pulp.System.FFI
import Pulp.System.Stream

foreign import argv' "var argv$prime = process.argv" :: [String]

commandName :: String
commandName = fromMaybe "pulp" $ argv' !! 1

argv :: [String]
argv = drop 2 argv'

foreign import exit """
  function exit(code) {
    return function() {
      process.exit(code);
    };
  }""" :: forall e. Number -> EffN e Unit

foreign import stdin "var stdin = process.stdin" :: NodeStream String
foreign import stdout "var stdout = process.stdout" :: NodeStream String
foreign import stderr "var stderr = process.stderr" :: NodeStream String
