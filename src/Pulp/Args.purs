module Pulp.Args where

import Control.Monad.Aff
import Data.Map (Map(..))
import Data.Maybe (Maybe(..))

import Text.Parsing.Parser (ParserT())

import Pulp.System.FFI

type Options = Map String (Maybe String)

type Action = forall e. Options -> Aff e Unit

type OptParser a = forall e. ParserT [String] (Aff (node :: Node | e)) a

type OptionParser = {
  name :: Maybe String,
  parser :: String -> OptParser (Maybe String)
  }

type Option = {
  name :: String,
  match :: [String],
  parser :: OptionParser,
  desc :: String,
  defaultValue :: Maybe String
  }

type Command = {
  name :: String,
  desc :: String,
  options :: [Option],
  action :: Action
  }

type Args = {
  globalOpts :: Options,
  commandOpts :: Options,
  command :: Command,
  remainder :: [String]
  }

option :: String -> [String] -> OptionParser -> String -> Option
option name match parser desc = {
  name: name,
  match: match,
  parser: parser,
  desc: desc,
  defaultValue: Nothing
  }

optionDefault :: String -> [String] -> OptionParser -> String -> String -> Option
optionDefault n m p d defaultValue =
  (option n m p d) { defaultValue = Just defaultValue }

command :: String -> String -> Action -> [Option] -> Command
command name desc action options = {
  name: name,
  desc: desc,
  options: options,
  action: action
  }
