module Pulp.Validate
  ( validate
  , getPursVersion
  , getPsaVersion
  ) where

import Prelude
import Data.Maybe
import Data.Either
import Data.List (fromFoldable, List(..))
import Data.String (trim, takeWhile)
import Data.Version.Haskell (Version(..), parseVersion, showVersion)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Text.Parsing.Parser (parseErrorMessage)

import Pulp.Exec (execQuiet)
import Pulp.System.FFI
import Pulp.Outputter (Outputter())

validate :: Outputter -> AffN Version
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

getPursVersion :: Outputter -> AffN Version
getPursVersion = getVersionFrom "purs"

minimumPursVersion :: Version
minimumPursVersion = Version (fromFoldable [0, 11, 0]) Nil

getPsaVersion :: Outputter -> AffN Version
getPsaVersion = getVersionFrom "psa"

getVersionFrom :: String -> Outputter -> AffN Version
getVersionFrom bin out = do
  verStr <- takeWhile (_ /= ' ') <$> trim <$> execQuiet bin ["--version"] Nothing
  case parseVersion verStr of
    Right v ->
      pure v
    Left err -> do
      let msg = parseErrorMessage err
      out.err $ "Unable to parse the version from " <> bin <> ". (It was: " <> verStr <> ")"
      out.err $ "Please check that the right executable is on your PATH."
      throwError $ error ("Couldn't parse version from " <> bin)
