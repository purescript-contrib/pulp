module Pulp.System.Process
       ( commandName
       , argv
       , exit
       , stdin
       , stdout
       , stderr
       , getEnvironment
       , getEnv
       , setEnv
       , getPlatform
       , chdir
       , cwd
       , __dirname
       ) where

import Prelude

import Data.Nullable (toMaybe, Nullable())
import Data.Array (drop, (!!))
import Data.Maybe (fromMaybe, Maybe())
import Data.StrMap (StrMap())
import Node.Stream (Writable(), Readable())

import Pulp.System.FFI
import Pulp.System.Stream

foreign import argv' :: Array String

commandName :: String
commandName = fromMaybe "pulp" $ argv' !! 1

argv :: Array String
argv = drop 2 argv'

foreign import exit :: forall a. Int -> EffN a

foreign import stdin  :: ReadableStream String
foreign import stdout :: WritableStream String
foreign import stderr :: WritableStream String

-- | Gets a copy of the current environment
foreign import getEnvironment :: EffN (StrMap String)

-- | Get a specific value out of the environment
foreign import getEnvNullable :: String -> EffN (Nullable String)

getEnv :: String -> EffN (Maybe String)
getEnv var = toMaybe <$> getEnvNullable var

-- | Set a specific value in the environment
foreign import setEnv :: String -> String -> EffN Unit

foreign import getPlatform :: EffN String

foreign import chdir :: String -> EffN Unit

foreign import cwd :: EffN String

foreign import __dirname :: String
