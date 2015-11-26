-- | Functions for getting data back out of an `Options` value.
module Pulp.Args.Get
  ( getOption
  , getOption'
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

-- | Get an option out of the `Options` value. If the option has no default and
-- | was not specified at the command line, the result will be `Nothing`. For
-- | options which do have defaults, you probably want the primed version
-- | instead, `getOption'`.
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

-- | Get an option which was declared with a default value, and therefore
-- | should always have a value.
getOption' :: forall e a. (IsForeign a) => String -> Options -> AffN e a
getOption' name opts = do
  mval <- getOption name opts
  case mval of
    Just val ->
      pure val
    Nothing ->
      let msg = "Missing default value for option: " ++ name
      in internalError msg (error msg)

-- | Get a flag out of the `Options` value. If it was specified at the command
-- | line, the result is `true`, otherwise, `false`.
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
