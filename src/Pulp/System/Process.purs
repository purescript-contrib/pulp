module Pulp.System.Process
       ( commandName
       , argv
       , exit
       ) where

import Control.Monad.Eff (Eff(..))

import Data.Array (drop, (!!))
import Data.Maybe (fromMaybe)

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
  }""" :: forall e. Number -> Eff e Unit
