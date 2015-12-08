module Pulp.Validate (validate) where

import Prelude
import Data.Maybe
import Data.Either
import Data.List (toList, List(..))
import Data.String (trim)
import Data.Version.Haskell (Version(..), parseVersion, showVersion)
import Control.Monad (when)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Text.Parsing.Parser (ParseError(..))

import Pulp.Exec (execQuiet)
import Pulp.System.FFI
import Pulp.Outputter (Outputter())

validate :: forall e. Outputter e -> AffN e Unit
validate out = do
  ver <- getPscVersion out
  when (ver < minimumPscVersion) $ do
    out.err $ "This version of Pulp requires PureScript version "
              <> showVersion minimumPscVersion <> " or higher."
    out.err $ "Your installed version is " <> showVersion ver <> "."
    out.err $ "Please either upgrade PureScript or downgrade Pulp to version 3.x."
    throwError $ error "Minimum psc version not satisfied"

getPscVersion :: forall e. Outputter e -> AffN e Version
getPscVersion out = do
  verStr <- trim <$> execQuiet "psc" ["--version"] Nothing
  case parseVersion verStr of
    Right v ->
      pure v
    Left (ParseError { message: msg }) -> do
      out.err $ "Unable to parse the version from psc. (It was: " <> verStr <> ")"
      out.err $ "Please check that the right psc is on your PATH."
      throwError $ error "Couldn't parse version from psc"

minimumPscVersion :: Version
minimumPscVersion = Version (toList [0, 7, 0, 0]) Nil
