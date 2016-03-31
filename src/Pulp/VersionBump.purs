-- | Defines a VersionBump type and associated functions.
module Pulp.VersionBump where

import Prelude
import Data.Maybe
import Data.Either
import Data.String as String
import Data.Version (Version)
import Data.Version as Version

data VersionBump = Major | Minor | Patch | ToExact Version

instance showBump :: Show VersionBump where
  show Major = "Major"
  show Minor = "Minor"
  show Patch = "Patch"
  show (ToExact v) = "(ToExact " <> Version.showVersion v <> ")"

parseBump :: String -> Maybe VersionBump
parseBump str =
  case String.toLower str of
    "major" -> Just Major
    "minor" -> Just Minor
    "patch" -> Just Patch
    _ -> ToExact <$> either (const Nothing) Just (Version.parseVersion str)

applyBump :: VersionBump -> Version -> Version
applyBump b = case b of
  Major -> Version.bumpMajor
  Minor -> Version.bumpMinor
  Patch -> Version.bumpPatch
  ToExact v -> const v 
