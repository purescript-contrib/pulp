-- | Functions for getting data back out of an `Options` value.
module Pulp.Args.Get
  ( getOption
  , getOption'
  , getFlag
  , hasOption
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..), isJust)
import Data.String (joinWith)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Foreign (Foreign)
import Foreign.Class (class Decode, decode)
import Pulp.Args (Options)
import Pulp.System.FFI (unsafeInspect)

-- | Get an option out of the `Options` value. If the option has no default and
-- | was not specified at the command line, the result will be `Nothing`. For
-- | options which do have defaults, you probably want the primed version
-- | instead, `getOption'`.
getOption :: forall a. Decode a => String -> Options -> Aff (Maybe a)
getOption name opts = do
  case lookup name opts of
    Just (Just thing) ->
      Just <$> readForeign name thing
    Just Nothing ->
      let msg = "Tried to read a flag as an option: " <> name
      in internalError msg
    Nothing ->
      pure Nothing

-- | Get an option which was declared with a default value, and therefore
-- | should always have a value.
getOption' :: forall a. Decode a => String -> Options -> Aff a
getOption' name opts = do
  mval <- getOption name opts
  case mval of
    Just val ->
      pure val
    Nothing ->
      let msg = "Missing default value for option: " <> name
      in internalError msg

-- | Get a flag out of the `Options` value. If it was specified at the command
-- | line, the result is `true`, otherwise, `false`.
getFlag :: String -> Options -> Aff Boolean
getFlag name opts = do
  case lookup name opts of
    Just (Just _) ->
      let msg = "Tried to read an option as a flag: " <> name
      in internalError msg
    Just Nothing ->
      pure true
    Nothing ->
      pure false

-- | True if a given option exists in the `Options` map, false otherwise.
hasOption :: String -> Options -> Aff Boolean
hasOption name opts = isJust <$> opt
  where
  opt :: Aff (Maybe Foreign)
  opt = getOption name opts

readForeign :: forall a. Decode a => String -> Foreign -> Aff a
readForeign name thing =
  case runExcept (decode thing) of
    Left e ->
      internalError $ joinWith "\n"
        [ "Failed to read option: " <> name
        , "The value was: " <> unsafeInspect thing
        , "Foreign.read failed: " <> show e
        ]
    Right x ->
      pure x

internalError :: forall b. String -> Aff b
internalError msg =
  throwError (error
    ("Internal error in Pulp.Args.Get: " <> msg <> "\n" <>
     "This is a bug. Please report it.\n"))
