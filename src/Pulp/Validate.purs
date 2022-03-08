module Pulp.Validate
  ( validate
  , getPursVersion
  , getPsaVersion
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar, takeWhile, trim)
import Data.Version.Haskell (Version(..), parseVersion, showVersion)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Pulp.Exec (execQuiet)
import Pulp.Outputter (Outputter)

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
minimumPursVersion = Version (NEL.cons' 0 (Cons 12 (Cons 0 Nil))) Nil

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
