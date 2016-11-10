
module Pulp.Project
  ( Project(..)
  , getProject
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Control.Monad.Except (runExcept)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign (Foreign(), parseJSON)
import Data.Foreign.Class (class IsForeign, readProp)

import Node.FS.Aff (exists, readTextFile)
import Node.Encoding (Encoding(UTF8))
import Node.Path as P
import Node.Process as Process

import Pulp.System.FFI
import Pulp.System.Files (mkdirIfNotExist)
import Pulp.Args (Options())
import Pulp.Args.Get (getOption)

newtype Project = Project
  { bowerFile :: Foreign
  , path :: String
  , cache :: String
  }

-- | Attempt to find a file in the given directory or any parent of it.
findIn :: String -> String -> AffN (Maybe String)
findIn path file = do
  let fullPath = P.concat [path, file]
  doesExist <- exists fullPath

  if doesExist
    then pure (Just fullPath)
    else
      let parent = P.dirname path
      in if path == parent
           then pure Nothing
           else findIn parent file

-- | Read a project's bower file at the given path and construct a Project
-- | value.
readConfig :: String -> AffN Project
readConfig configFilePath = do
  json <- readTextFile UTF8 configFilePath
  case runExcept (parseJSON json) of
    Left err ->
      throwError (error ("Unable to parse bower.json: " <> show err))
    Right pro -> do
      let path = P.dirname configFilePath
      let cachePath = P.resolve [path] ".pulp-cache"
      liftEff $ Process.chdir path
      mkdirIfNotExist cachePath
      pure $ Project { bowerFile: pro, cache: cachePath, path: path }

-- | Use the provided bower file, or if it is Nothing, try to find a bower file
-- | path in this or any parent directory.
getBowerFile :: Maybe String -> AffN String
getBowerFile = maybe search pure
  where
  search = do
    cwd <- liftEff Process.cwd
    mbowerFile <- findIn cwd "bower.json"
    case mbowerFile of
      Just bowerFile -> pure bowerFile
      Nothing -> throwError <<< error $
        "No bower.json found in current or parent directories. Are you in a PureScript project?"

getProject :: Options -> AffN Project
getProject args =
  getOption "bowerFile" args >>= getBowerFile >>= readConfig

instance isForeignProject :: IsForeign Project where
  read o =
    map Project $ do
      bowerFile <- readProp "bowerFile" o
      path      <- readProp "path" o
      cache     <- readProp "cache" o
      pure $ { bowerFile, path, cache }
