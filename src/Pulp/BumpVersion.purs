module Pulp.BumpVersion ( action ) where

import Prelude
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Aff
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Array as Array
import Data.List (List(..))
import Data.Foldable as Foldable
import Data.Version (Version)
import Data.Version as Version
import Data.String as String
import Node.ChildProcess as CP

import Pulp.System.FFI
import Pulp.Exec
import Pulp.VersionBump
import Pulp.System.Read as Read
import Pulp.Outputter
import Pulp.Args
import Pulp.Args.Get

action :: Action
action = Action \args -> do
  out <- getOutputter args

  requireCleanGitWorkingTree
  checkPscPublish out
  version <- bumpVersion args
  tagNewVersion version
  out.log ("Bumped to: v" <> Version.showVersion version)

-- | Throw an error if the git working tree is dirty.
requireCleanGitWorkingTree :: AffN Unit
requireCleanGitWorkingTree = do
  out <- execQuiet "git" ["status", "--porcelain"] Nothing
  if Foldable.all String.null (String.split "\n" out)
    then pure unit
    else throwError <<< error $
      "Your git working tree is dirty. Please commit or stash your changes " <>
      "first."

-- | Try running `psc-publish --dry-run` to make sure the code is suitable for
-- | release.
checkPscPublish :: Outputter -> AffN Unit
checkPscPublish out = do
  out.log "Checking your package using psc-publish..."
  exec "psc-publish" ["--dry-run"] Nothing

-- | Returns the new version that we should bump to.
bumpVersion :: Args -> AffN Version
bumpVersion args = do
  mbumpStr <- getOption "bump" args.commandArgs
  mcurrent <- getVersionFromGitTag
  out <- getOutputter args

  case mbumpStr of
    Just bumpStr -> do
      -- TODO: this is gross. See also Pulp.Args.Types.versionBump
      bump <- maybe (internalError "invalid bump") pure (parseBump bumpStr)
      maybe (promptInitial out) (pure <<< applyBump bump) mcurrent
    Nothing ->
      maybe (promptInitial out) (promptCurrent out) mcurrent

tagNewVersion :: Version -> AffN Unit
tagNewVersion version = do
  let versionStr = "v" <> Version.showVersion version
  exec "git"
    [ "commit"
    , "--allow-empty"
    , "--message=" <> versionStr
    ] Nothing
  exec "git"
    [ "tag"
    , "--annotate"
    , "--message=" <> versionStr 
    , versionStr
    ] Nothing

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

-- | Prompt and ask the user what to use as the initial version.
promptInitial :: Outputter -> AffN Version
promptInitial out = do
  out.log "Initial version"
  out.write "You can release this code as:\n"

  Foldable.for_ initialOptions \(Tuple letter version) ->
    out.write (letter <> ") v" <> Version.showVersion version <> "\n")

  untilJust do
    choice <- Read.read { prompt: "Choose one, or enter a specific version:"
                        , silent: false
                        }

    case lookup (String.toLower choice) initialOptions of
      Just v ->
        pure (Just v)
      Nothing ->
        case Version.parseVersion (dropPrefix "v" choice) of
          Right v ->
            pure (Just v)
          Left _ -> do
            out.log "Sorry, that choice wasn't understood."
            pure Nothing

  where
  initialOptions =
    [ Tuple "a" (vers 1 0 0)
    , Tuple "b" (vers 0 1 0)
    , Tuple "c" (vers 0 0 1)
    ]

  vers major minor patch = Version.version major minor patch Nil Nil

promptCurrent :: Outputter -> Version -> AffN Version
promptCurrent out current = do
  out.log ("The current version is v" <> Version.showVersion current)
  out.write "You can bump the version to:\n"

  Foldable.for_ bumpOptions \(Tuple letter bump) ->
    out.write (letter <> ") v" <> Version.showVersion (applyBump bump current) <> "\n")

  untilJust do
    choice <- Read.read { prompt: "Choose one, or enter a specific version:"
                        , silent: false
                        }

    case lookup (String.toLower choice) bumpOptions of
      Just bump ->
        pure (Just (applyBump bump current))
      Nothing ->
        case Version.parseVersion (dropPrefix "v" choice) of
          Right v ->
            pure (Just v)
          Left _ -> do
            out.log "Sorry, that choice wasn't understood."
            pure Nothing

  where
  bumpOptions =
    [ Tuple "a" Major
    , Tuple "b" Minor
    , Tuple "c" Patch
    ]

untilJust :: forall m a. (Monad m) => m (Maybe a) -> m a
untilJust act = do
  val <- act
  case val of
    Just x ->
      pure x
    Nothing ->
      untilJust act

internalError :: forall a. String -> AffN a
internalError msg =
  throwError <<< error $
    "Internal error in Pulp.BumpVersion: " <> msg <> "\n" <>
    "This is a bug. Please report it.\n"
