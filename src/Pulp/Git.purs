module Pulp.Git where

import Prelude
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Aff
import Data.Maybe
import Data.Either
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Version (Version)
import Data.Version as Version
import Data.String as String
import Node.ChildProcess as CP

import Pulp.System.FFI
import Pulp.Exec

-- | Throw an error if the git working tree is dirty.
requireCleanGitWorkingTree :: AffN Unit
requireCleanGitWorkingTree = do
  out <- execQuiet "git" ["status", "--porcelain"] Nothing
  if Foldable.all String.null (String.split "\n" out)
    then pure unit
    else throwError <<< error $
      "Your git working tree is dirty. Please commit or stash your changes " <>
      "first."

-- | Get the most recently tagged version from the git tag.
getVersionFromGitTag :: AffN (Maybe Version)
getVersionFromGitTag =
  attempt (run "git" ["describe", "--tags", "--abbrev=0", "HEAD"] Nothing)
  # map (either (const Nothing) maxVersion)

 where
 -- Run a command, piping stderr to /dev/null
 run = execQuietWithStderr CP.Ignore

-- | Given a number of lines of text, attempt to parse each line as a version,
-- | and return the maximum.
maxVersion :: String -> Maybe Version
maxVersion =
  String.split "\n"
  >>> Array.mapMaybe (String.trim >>> parseMay)
  >>> Foldable.maximum

  where
  parseMay =
    dropPrefix "v"
    >>> Version.parseVersion
    >>> either (const Nothing) Just

dropPrefix :: String -> String -> String
dropPrefix prefix str =
  fromMaybe str (String.stripPrefix prefix str)
