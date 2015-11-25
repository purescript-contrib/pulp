module Pulp.Args where

import Prelude

import Control.Bind (join)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, Error())
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Data.Either (either)
import Data.Map (Map(), lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.List (List(..))
import Data.Foreign (Foreign())
import Data.Foreign.Class (IsForeign, read)

import Text.Parsing.Parser (ParserT())
import Node.FS (FS())

import Pulp.System.FFI
import qualified Pulp.System.Log as Log

type Options = Map String (Maybe Foreign)

type Action = forall e. Options -> Aff e Unit

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
  defaultValue :: Maybe String
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

optionDefault :: String -> Array String -> OptionParser -> String -> String -> Option
optionDefault n m p d defaultValue =
  (option n m p d) { defaultValue = Just defaultValue }

command :: String -> String -> Action -> Array Option -> Command
command name desc action options = {
  name: name,
  desc: desc,
  options: options,
  action: action
  }

getOption :: forall e a. (IsForeign a) => String -> Options -> AffN e (Maybe a)
getOption name opts = do
  case lookup name opts of
    Just opt ->
      maybe (pure Nothing) (map Just <<< fToAff <<< read) opt
    Nothing ->
      let msg = ("Tried to read a flag as an option: (" ++ name ++ ").")
      in internalError msg (error msg)
  where
  fToAff = either (internalError "Data.Foreign.read failed" <<< error <<< show) return

  internalError :: forall b. String -> Error -> AffN e b
  internalError msg err = do
    Log.err $ "Internal error: Pulp.Args.getOption: " ++ msg
    Log.err "This is a bug. Please report it."
    throwError err
