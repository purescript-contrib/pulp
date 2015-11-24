module Pulp.Args.Types
  ( flag
  , string
  , file
  , int
  , directory
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Alt
import Control.Monad.Trans
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Int (fromString)
import Data.Foldable (elem)

import Text.Parsing.Parser (ParserT(..))
import Node.FS (FS())
import Node.FS.Aff (exists)

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
  parser: \arg -> Just <$> (token <|> argErr arg "Needs a string argument.")
  }

int :: OptionParser
int = {
  name: Just "<int>",
  parser: \arg -> do
    let err = argErr arg "Needs an int argument." :: forall a. OptParser a
    mint <- fromString <$> (token <|> err)
    case mint of
      Just i -> return (Just (show i))
      Nothing -> err
  }

file :: OptionParser
file = {
  name: Just "<file>",
  parser: \arg -> do
    path <- token <|> argErr arg "Needs a file argument."
    e <- lift $ exists path
    if e then return $ Just path
      else halt ("File '" ++ path ++ "' not found.")
  }

directory :: OptionParser
directory = {
  name: Just "<directory>",
  parser: \arg -> do
    path <- token <|> argErr arg "Needs a directory argument."
    return $ Just "lol"
  }
