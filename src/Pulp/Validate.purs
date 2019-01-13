module Pulp.Validate
  ( validate
  , getPursVersion
  , getPsaVersion
  ) where

import Data.Either
import Data.Maybe
import Prelude
import Pulp.System.FFI

import Control.Monad.Error.Class (throwError)
import Data.List (fromFoldable, List(..))
import Data.String (codePointFromChar, takeWhile, trim)
import Data.Version.Haskell (Version(..), parseVersion, showVersion)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Pulp.Exec (execQuiet)
import Pulp.Outputter (Outputter)
import Text.Parsing.Parser (parseErrorMessage)

validate :: Outputter -> Aff Version
validate out = do
  ver <- getPursVersion out
  when (ver < minimumPursVersion) $ do
    out.err $ "This version of Pulp requires version "
              <> showVersion minimumPursVersion <> " of the PureScript compiler "
              <> "or higher."
    out.err $ "Your installed version is " <> showVersion ver <> "."
    out.err $ "Please either upgrade PureScript or downgrade Pulp to version 10.x."
    throwError $ error "Minimum purs version not satisfied"
  pure ver

getPursVersion :: Outputter -> Aff Version
getPursVersion = getVersionFrom "purs"

minimumPursVersion :: Version
minimumPursVersion = Version (fromFoldable [0, 11, 0]) Nil

getPsaVersion :: Outputter -> Aff Version
getPsaVersion = getVersionFrom "psa"

getVersionFrom :: String -> Outputter -> Aff Version
getVersionFrom bin out = do
  verStr <- takeWhile (_ /= codePointFromChar ' ') <$> trim <$> execQuiet bin ["--version"] Nothing
  case parseVersion verStr of
    Right v ->
      pure v
    Left err -> do
      let msg = parseErrorMessage err
      out.err $ "Unable to parse the version from " <> bin <> ". (It was: " <> verStr <> ")"
      out.err $ "Please check that the right executable is on your PATH."
      throwError $ error ("Couldn't parse version from " <> bin)
