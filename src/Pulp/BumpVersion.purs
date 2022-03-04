module Pulp.BumpVersion ( action ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (lookup)
import Data.Foldable as Foldable
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Tuple (Tuple(..), snd)
import Data.Version (Version)
import Data.Version as Version
import Effect.Aff (Aff)
import Effect.Exception (error)
import Pulp.Args (Action(..), Args)
import Pulp.Args.Get (getOption)
import Pulp.Exec (exec)
import Pulp.Git (dropPrefix, getLatestTaggedVersion, requireCleanGitWorkingTree)
import Pulp.Outputter (Outputter, getOutputter)
import Pulp.Publish (resolutionsFile, parseJsonFile, BowerJson)
import Pulp.System.Read as Read
import Pulp.VersionBump (VersionBump(..), applyBump, parseBump)

action :: Action
action = Action \args -> do
  out <- getOutputter args

  requireCleanGitWorkingTree
  checkPursPublish args
  version <- bumpVersion args
  tagNewVersion version
  out.log ("Bumped to: v" <> Version.showVersion version)

-- | Try running `purs publish --dry-run` to make sure the code is suitable for
-- | release.
checkPursPublish :: Args -> Aff Unit
checkPursPublish args = do
  out <- getOutputter args
  out.log "Checking your package using purs publish..."
  manifest :: BowerJson <- parseJsonFile "bower.json"
  resolutions <- resolutionsFile manifest args
  exec
    "purs"
    ["publish", "--manifest", "bower.json", "--resolutions", resolutions, "--dry-run"]
    Nothing

-- | Returns the new version that we should bump to.
bumpVersion :: Args -> Aff Version
bumpVersion args = do
  out <- getOutputter args
  mcurrent <- map (map snd) getLatestTaggedVersion
  mbumpStr <- getOption "bump" args.commandArgs
  mbump <- case mbumpStr of
    -- TODO: this is gross. See also Pulp.Args.Types.versionBump
    Just bumpStr -> maybe (internalError "invalid bump") (pure <<< Just) (parseBump bumpStr)
    Nothing -> pure Nothing

  newVersion mbump mcurrent out

newVersion :: Maybe VersionBump -> Maybe Version -> Outputter -> Aff Version
newVersion mbump mcurrent out = case mcurrent, mbump of
  _,            Just (ToExact version) -> pure version
  Just current, Just bump              -> pure $ applyBump bump current
  Just current, Nothing                -> promptCurrent out current
  Nothing     , _                      -> promptInitial out

tagNewVersion :: Version -> Aff Unit
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

-- | Prompt and ask the user what to use as the initial version.
promptInitial :: Outputter -> Aff Version
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

promptCurrent :: Outputter -> Version -> Aff Version
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

internalError :: forall a. String -> Aff a
internalError msg =
  throwError <<< error $
    "Internal error in Pulp.BumpVersion: " <> msg <> "\n" <>
    "This is a bug. Please report it.\n"
