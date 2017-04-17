module Pulp.Args.Types
  ( flag
  , string
  , file
  , int
  , directory
  , directories
  , versionBump
  ) where

import Prelude

import Control.Alt
import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.String (null, split, Pattern(..))
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.Foldable (for_)
import Data.Foreign (toForeign)
import Text.Parsing.Parser (fail)

import Node.FS.Stats (Stats(), isFile, isDirectory)
import Node.FS.Aff (stat)
import Node.Path as Path

import Pulp.Args
import Pulp.Args.Parser
import Pulp.VersionBump

argErr :: forall a. String -> String -> OptParser a
argErr arg msg =
  halt ("Argument " <> arg <> ": " <> msg)

flag :: OptionParser
flag = {
  name: Nothing,
  parser: \_ -> pure Nothing
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
      Just i -> pure (Just (toForeign i))
      Nothing -> err
  }

require :: (Stats -> Boolean) -> String -> String -> OptParser Unit
require pred typ path = do
  s <- lift (stat path) <|> halt (typ <> " '" <> path <> "' not found.")
  unless (pred s)
    (halt ("Path '" <> path <> "' is not a " <> typ <> "."))

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
    pure $ Just (toForeign path)
  }

directory :: OptionParser
directory = {
  name: Just "<dir>",
  parser: \arg -> do
    path <- token <|> argErr arg "Needs a directory argument."
    requireDirectory path
    pure $ Just (toForeign path)
  }

directories :: OptionParser
directories = {
  name: Just "<dir:dir:...>",
  parser: \arg -> do
    paths <- token <|> argErr arg "Needs a directory argument."
    let paths' = filter (not <<< null) $ split (Pattern Path.delimiter) paths
    for_ paths' requireDirectory
    pure $ Just (toForeign paths')
  }

-- TODO: this is gross; we end up parsing the version twice. Probably should
-- fix this by parameterising OptionParsers and ArgumentParsers based on the
-- type of the thing they parse.
versionBump :: ArgumentParser
versionBump arg =
  case parseBump arg of
    Just _ ->
      pure (toForeign arg)
    Nothing ->
      fail ("Not a valid version bump. Must be: 'major', 'minor', 'patch', "
            <> "or a version.")
