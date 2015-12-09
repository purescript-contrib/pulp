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

import Pulp.System.FFI
import Pulp.System.Stream

foreign import argv' :: Array String

commandName :: String
commandName = fromMaybe "pulp" $ argv' !! 1

argv :: Array String
argv = drop 2 argv'

foreign import exit :: forall e a. Int -> EffN a

foreign import stdin :: NodeStream String
foreign import stdout :: NodeStream String
foreign import stderr :: NodeStream String

-- | Gets a copy of the current environment
foreign import getEnvironment :: forall e. EffN (StrMap String)

-- | Get a specific value out of the environment
foreign import getEnvNullable :: forall e. String -> EffN (Nullable String)

getEnv :: forall e. String -> EffN (Maybe String)
getEnv var = toMaybe <$> getEnvNullable var

-- | Set a specific value in the environment
foreign import setEnv :: forall e. String -> String -> EffN Unit

foreign import getPlatform :: forall e. EffN String

foreign import chdir :: forall e. String -> EffN Unit

foreign import cwd :: forall e. EffN String

foreign import __dirname :: String
