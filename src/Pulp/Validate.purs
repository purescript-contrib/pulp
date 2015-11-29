module Pulp.Validate (validate) where

import Prelude
import Data.Maybe
import Data.Either
import Data.List (toList, List(..))
import Data.String (trim)
import Data.Version.Haskell (Version(..), parseVersion, showVersion)
import Control.Monad (when)
import Control.Monad.Eff.Class (liftEff)
import Text.Parsing.Parser (ParseError(..))

import Pulp.Exec (execQuiet)
import Pulp.System.Process (exit)
import Pulp.System.FFI
import Pulp.System.Log as Log

validate :: forall e. AffN e Unit
validate = do
  ver <- getPscVersion
  when (ver < minimumPscVersion) $ do
    Log.err $ "This version of Pulp requires PureScript version "
              <> showVersion minimumPscVersion <> " or higher."
    Log.err $ "Your installed version is " <> showVersion ver <> "."
    Log.err $ "Please either upgrade PureScript or downgrade Pulp to version 3.x."
    liftEff $ exit 1

getPscVersion :: forall e. AffN e Version
getPscVersion = do
  verStr <- trim <$> execQuiet "psc" ["--version"] Nothing
  case parseVersion verStr of
    Right v ->
      pure v
    Left (ParseError { message: msg }) -> do
      Log.err $ "Unable to parse the version from psc. (It was: " <> verStr <> ")"
      Log.err $ "Please check that the right psc is on your PATH."
      liftEff $ exit 1

minimumPscVersion :: Version
minimumPscVersion = Version (toList [0, 7, 0, 0]) Nil
