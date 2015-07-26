module Pulp.Args.Types where

import Prelude

import Control.Alt
import Control.Monad.Trans
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Text.Parsing.Parser (ParserT(..))

import Pulp.Args
import Pulp.Args.Parser
import Pulp.System.Files (exists)

flag :: OptionParser
flag = {
  name: Nothing,
  parser: \_ -> return Nothing
  }

string :: OptionParser
string = {
  name: Just "<string>",
  parser: \arg -> do
    val <- token <|> halt ("Argument " ++ arg ++ ": Needs a string argument.")
    return $ Just val
  }

file = {
  name: Just "<file>",
  parser: \arg -> do
    path <- string.parser arg
    case path of
      Just path -> do
        e <- lift $ exists path
        if e then return $ Just path
          else halt ("File '" ++ path ++ "' not found.")
  }
