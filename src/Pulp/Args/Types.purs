module Pulp.Args.Types
  ( flag
  , string
  , file
  , int
  , directory
  , directories
  ) where

import Prelude

import Control.Monad (unless)
import Control.Alt
import Control.Monad.Trans
import Data.String (split)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.Foldable (for_)
import Data.Foreign (toForeign)

import Node.FS.Stats (Stats(), isFile, isDirectory)
import Node.FS.Aff (stat)
import qualified Node.Path as Path

import Pulp.Args
import Pulp.Args.Parser

argErr :: forall a. String -> String -> OptParser a
argErr arg msg =
  halt ("Argument " ++ arg ++ ": " ++ msg)

flag :: OptionParser
flag = {
  name: Nothing,
  parser: \_ -> return Nothing
  }

string :: OptionParser
string = {
  name: Just "<string>",
  parser: \arg ->
    (Just <<< toForeign) <$> (token <|> argErr arg "Needs a string argument.")
  }

int :: OptionParser
int = {
  name: Just "<int>",
  parser: \arg -> do
    let err = argErr arg "Needs an int argument." :: forall a. OptParser a
    mint <- fromString <$> (token <|> err)
    case mint of
      Just i -> return (Just (toForeign i))
      Nothing -> err
  }

require :: (Stats -> Boolean) -> String -> String -> OptParser Unit
require pred typ path = do
  s <- lift (stat path) <|> halt (typ ++ " '" ++ path ++ "' not found.")
  unless (pred s)
    (halt ("Path '" ++ path ++ "' is not a " ++ typ ++ "."))

requireFile :: String -> OptParser Unit
requireFile = require isFile "File"

requireDirectory :: String -> OptParser Unit
requireDirectory = require isDirectory "Directory"

file :: OptionParser
file = {
  name: Just "<file>",
  parser: \arg -> do
    path <- token <|> argErr arg "Needs a file argument."
    requireFile path
    return $ Just (toForeign path)
  }

directory :: OptionParser
directory = {
  name: Just "<directory>",
  parser: \arg -> do
    path <- token <|> argErr arg "Needs a directory argument."
    requireDirectory path
    return $ Just (toForeign path)
  }

directories :: OptionParser
directories = {
  name: Just "<directories>",
  parser: \arg -> do
    paths <- token <|> argErr arg "Needs a directory argument."
    let paths' = split Path.delimiter paths
    for_ paths' requireDirectory
    return $ Just (toForeign paths')
  }
