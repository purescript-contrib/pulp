-- | Functions for getting data back out of an `Options` value.
module Pulp.Args.Get
  ( getOption
  , getFlag
  ) where

import Prelude
import Data.Either
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Data.Map (lookup)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, Error())

import Pulp.System.FFI
import Pulp.Args
import qualified Pulp.System.Log as Log

getOption :: forall e a. (IsForeign a) => String -> Options -> AffN e (Maybe a)
getOption name opts = do
  case lookup name opts of
    Just (Just thing) ->
      Just <$> fToAff (read thing)
    Just Nothing ->
      let msg = "Tried to read a flag as an option: " ++ name
      in internalError msg (error msg)
    Nothing ->
      pure Nothing

getFlag :: forall e. String -> Options -> AffN e Boolean
getFlag name opts = do
  case lookup name opts of
    Just (Just _) ->
      let msg = "Tried to read an option as a flag: " ++ name
      in internalError msg (error msg)
    Just Nothing ->
      pure true
    Nothing ->
      pure false

fToAff :: forall e a. F a -> AffN e a
fToAff = either (internalError "Data.Foreign.read failed" <<< error <<< show) return

internalError :: forall e b. String -> Error -> AffN e b
internalError msg err = do
  Log.err $ "Internal error: Pulp.Args.getOption: " ++ msg
  Log.err "This is a bug. Please report it."
  throwError err
