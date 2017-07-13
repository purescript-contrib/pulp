module Pulp.Git
  ( requireCleanGitWorkingTree
  , getVersionFromGitTag
  , getLatestTaggedVersion
  , dropPrefix
  ) where

import Prelude
import Control.Monad.Aff (attempt)
import Data.Function
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Version (Version)
import Data.Version as Version
import Data.String as String
import Node.ChildProcess as CP

import Pulp.System.FFI
import Pulp.Exec
import Pulp.Utils (throw)

-- | Throw an error if the git working tree is dirty.
requireCleanGitWorkingTree :: AffN Unit
requireCleanGitWorkingTree = do
  out <- execQuiet "git" ["status", "--porcelain"] Nothing
  if Foldable.all String.null (String.split (String.Pattern "\n") out)
    then pure unit
    else throw ("Your git working tree is dirty. Please commit or stash " <>
                "your changes first.")

-- | Get the version tag pointing to the currently checked out commit, if any.
-- | The tag must start with a "v" and be followed by a valid semver version,
-- | for example "v1.2.3".
-- |
-- | If multiple tags point to the checked out commit, return the latest
-- | version according to semver version comparison.
getVersionFromGitTag :: AffN (Maybe (Tuple String Version))
getVersionFromGitTag = do
  output <- run "git" ["tag", "--points-at", "HEAD"]
  pure (maxVersion output)

-- | Get the latest semver version tag in the repository. The tag must start
-- | with a "v" and be followed by a valid semver version, for example
-- | "v1.2.3".
-- |
-- | Returns Nothing if there are no such tags in the repository.
getLatestTaggedVersion :: AffN (Maybe (Tuple String Version))
getLatestTaggedVersion = do
  output <- attempt $ run "git" ["describe", "--tags", "--abbrev=0", "HEAD"]
  pure $ either (const Nothing) maxVersion output

-- | Run a command, piping stderr to /dev/null
run :: String -> Array String -> AffN String
run cmd args = execQuietWithStderr CP.Ignore cmd args Nothing

-- | Given a number of lines of text, attempt to parse each line as a version,
-- | and return the maximum.
maxVersion :: String -> Maybe (Tuple String Version)
maxVersion =
  String.split (String.Pattern "\n")
  >>> Array.mapMaybe (String.trim >>> parseMay)
  >>> Foldable.maximumBy (compare `on` snd)

  where
  parseMay str =
    str
    # dropPrefix "v"
    # Version.parseVersion
    # either (const Nothing) Just
    # map (Tuple str)

dropPrefix :: String -> String -> String
dropPrefix prefix str =
  fromMaybe str (String.stripPrefix (String.Pattern prefix) str)
