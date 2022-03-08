module Pulp.Args where

import Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Text.Parsing.Parser (ParserT)

type Options = Map String (Maybe Foreign)

-- | Action is a newtype because a normal type synonym would lead to a loop,
-- | which is disallowed by the compiler.
newtype Action = Action (Args -> Aff Unit)

runAction :: Action -> Args -> Aff Unit
runAction (Action f) = f

type OptParser a = ParserT (List String) Aff a

newtype Help = Help Command

-- | We use Foreign for the result of the parser because we want to be able to
-- | put any type in at first. Then, we can use other functions in Foreign
-- | to get it out again, or throw an error if the types don't match.
-- |
-- | I expect there is a better way of doing this but this will do for now.
-- | It's no less safe than the JS implementation, at least.
type OptionParser = {
  name :: Maybe String,
  parser :: String -> OptParser (Maybe Foreign)
  }

type ArgumentParser = String -> OptParser Foreign

-- | A command line option, like `--output` or `--verbose`.
type Option = {
  name :: String,
  match :: Array String,
  parser :: OptionParser,
  desc :: String,
  defaultValue :: Maybe Foreign
  }

-- | A positional command line argument, like `major` in `pulp version
-- | major`.
type Argument = {
  name :: String,
  parser :: ArgumentParser,
  desc :: String,
  required :: Boolean
  }

type Command = {
  name :: String,
  desc :: String,
  alias :: Array String,
  passthroughDesc :: Maybe String,
  options :: Array Option,
  arguments :: Array Argument,
  action :: Action
  }

type Args = {
  globalOpts :: Options,
  commandOpts :: Options,
  commandArgs :: Options, -- TODO: this is a bit gross.
  command :: Command,
  remainder :: Array String
  }

option :: String -> Array String -> OptionParser -> String -> Option
option name match parser desc = {
  name,
  match,
  parser,
  desc,
  defaultValue: Nothing
  }

optionDefault :: forall a. String -> Array String -> OptionParser -> String -> a -> Option
optionDefault n m p d defaultValue =
  (option n m p d) { defaultValue = Just (unsafeToForeign defaultValue) }

argument :: String -> ArgumentParser -> String -> Boolean -> Argument
argument name parser desc required = {
  name,
  parser,
  desc,
  required
  }

command :: String -> String -> Maybe String -> Action -> Array Option -> Command
command name desc passthroughDesc action options = {
  name,
  desc,
  passthroughDesc,
  options,
  action,
  arguments: [],
  alias: []
  }

commandWithArgs :: String -> String -> Maybe String -> Action -> Array Option -> Array Argument -> Command
commandWithArgs name desc passthroughDesc action options args =
  (command name desc passthroughDesc action options) { arguments = args }

commandWithAlias :: String -> String -> Maybe String -> Action -> Array Option -> Array String -> Command
commandWithAlias name desc passthroughDesc action options alias =
  (command name desc passthroughDesc action options) { alias = alias }
