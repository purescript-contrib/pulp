module Pulp.Validate
  ( validate
  , getPursVersion
  , getPsaVersion
  , dropPreRelBuildMeta
  , failIfUsingEsModulesPsVersion
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar, takeWhile, trim)
import Data.Version.Haskell (Version(..), parseVersion, showVersion)
import Data.Version.Haskell as Version
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Pulp.Exec (execQuiet)
import Pulp.Outputter (Outputter)
import Pulp.Versions.PureScript (psVersions)

validate :: Outputter -> Aff Version
validate out = do
  ver <- getPursVersion out
  when (ver < minimumPursVersion) $ do
    out.err $ "This version of Pulp requires version "
              <> showVersion minimumPursVersion <> " of the PureScript compiler "
              <> "or higher."
    out.err $ "Your installed version is " <> showVersion ver <> "."
    out.err $ "Please either upgrade PureScript or downgrade Pulp to version 12.4.2."
    throwError $ error "Minimum purs version not satisfied"
  pure ver

getPursVersion :: Outputter -> Aff Version
getPursVersion = getVersionFrom "purs"

minimumPursVersion :: Version
minimumPursVersion = psVersions.v0_12_0

getPsaVersion :: Outputter -> Aff Version
getPsaVersion = getVersionFrom "psa"

getVersionFrom :: String -> Outputter -> Aff Version
getVersionFrom bin out = do
  verStr <- takeWhile (_ /= codePointFromChar ' ') <$> trim <$> execQuiet bin ["--version"] Nothing
  case parseVersion verStr of
    Right v ->
      pure v
    Left _ -> do
      out.err $ "Unable to parse the version from " <> bin <> ". (It was: " <> verStr <> ")"
      out.err $ "Please check that the right executable is on your PATH."
      throwError $ error ("Couldn't parse version from " <> bin)

failIfUsingEsModulesPsVersion :: Outputter -> Maybe String -> Aff Unit
failIfUsingEsModulesPsVersion out mbMsg = do
  psVer <- getPursVersion out
  unless ((dropPreRelBuildMeta psVer) < psVersions.v0_15_0) do
    out.err $ fold
      [ "This code path implicitly uses `purs bundle` or CommonsJS modules, both of which are no longer supported in PureScript v0.15.0."
      , "You are using PureScript " <> Version.showVersion psVer
      , "See https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md"
      ]
    for_ mbMsg out.err
    liftEffect $ throw $ "Your version of PureScript cannot use `purs bundle` or CommonJS modules. Please use another bundler (e.g. esbuild) instead."

dropPreRelBuildMeta :: Version -> Version
dropPreRelBuildMeta (Version mmp _) = Version mmp Nil
