module Pulp.Args where

import Prelude

import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Data.Map (Map())
import Data.Maybe (Maybe(..))
import Data.List (List())
import Data.Foreign (Foreign(), toForeign)

import Text.Parsing.Parser (ParserT())
import Node.FS (FS())

import Pulp.System.FFI

type Options = Map String (Maybe Foreign)

-- | Action is a newtype because a normal type synonym would lead to a loop,
-- | which is disallowed by the compiler.
newtype Action = Action (forall e. Args -> AffN Unit)

runAction :: forall e. Action -> Args -> AffN Unit
runAction (Action f) = f

type OptParser a = forall e. ParserT (List String) (Aff (fs :: FS, node :: Node, avar :: AVAR | e)) a

-- | We use Foreign for the result of the parser because we want to be able to
-- | put any type in at first. Then, we can use other functions in Data.Foreign
-- | to get it out again, or throw an error if the types don't match.
-- |
-- | I expect there is a better way of doing this but this will do for now.
-- | It's no less safe than the JS implementation, at least.
type OptionParser = {
  name :: Maybe String,
  parser :: String -> OptParser (Maybe Foreign)
  }

type Option = {
  name :: String,
  match :: Array String,
  parser :: OptionParser,
  desc :: String,
  defaultValue :: Maybe Foreign
  }

type Command = {
  name :: String,
  desc :: String,
  options :: Array Option,
  action :: Action
  }

type Args = {
  globalOpts :: Options,
  commandOpts :: Options,
  command :: Command,
  remainder :: Array String
  }

option :: String -> Array String -> OptionParser -> String -> Option
option name match parser desc = {
  name: name,
  match: match,
  parser: parser,
  desc: desc,
  defaultValue: Nothing
  }

optionDefault :: forall a. String -> Array String -> OptionParser -> String -> a -> Option
optionDefault n m p d defaultValue =
  (option n m p d) { defaultValue = Just (toForeign defaultValue) }

command :: String -> String -> Action -> Array Option -> Command
command name desc action options = {
  name: name,
  desc: desc,
  options: options,
  action: action
  }
