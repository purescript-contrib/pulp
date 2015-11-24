module Pulp.Args.Types
  ( flag
  , string
  , file
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Alt
import Control.Monad.Trans
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..))

import Text.Parsing.Parser (ParserT(..))
import Node.FS (FS())
import Node.FS.Aff (exists)

import Pulp.Args
import Pulp.Args.Parser

flag :: OptionParser
flag = {
  name: Nothing,
  parser: \_ -> return Nothing
  }

stringParser :: forall e.
  String -> ParserT (List String) (Aff (avar :: AVAR, node :: Node | e)) String
stringParser arg =
  token <|> halt ("Argument " ++ arg ++ ": Needs a string argument.")

string :: OptionParser
string = {
  name: Just "<string>",
  parser: \arg -> Just <$> stringParser arg
  }

file :: OptionParser
file = {
  name: Just "<file>",
  parser: \arg -> do
    path <- stringParser arg
    e <- lift $ exists path
    if e then return $ Just path
      else halt ("File '" ++ path ++ "' not found.")
  }
