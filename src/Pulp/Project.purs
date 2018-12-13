module Pulp.Project
  ( Project(..)
  , getProject
  , usingPscPackage
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (Foreign, readString)
import Foreign.Class (class Decode)
import Foreign.Index (readProp)
import Foreign.JSON (parseJSON)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (exists, readTextFile)
import Node.Path as P
import Node.Process as Process
import Pulp.Args (Options)
import Pulp.Args.Get (getOption, getFlag)
import Pulp.System.Files (mkdirIfNotExist)

newtype Project = Project
  { projectFile :: Foreign
  , path :: String
  , cache :: String
  }

-- | Attempt to find a file in the given directory or any parent of it.
findIn :: String -> String -> Aff (Maybe String)
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
readConfig :: String -> Aff Project
readConfig configFilePath = do
  json <- readTextFile UTF8 configFilePath
  case runExcept (parseJSON json) of
    Left err ->
      throwError (error ("Unable to parse " <> (P.basename configFilePath) <> ": " <> show err))
    Right pro -> do
      let path = P.dirname configFilePath
      cachePath <- liftEffect $ P.resolve [path] ".pulp-cache"
      liftEffect $ Process.chdir path
      mkdirIfNotExist cachePath
      pure $ Project { projectFile: pro, cache: cachePath, path: path }

-- | If project file has a `set` property we assume it's a psc-package project file
usingPscPackage :: Project -> Boolean
usingPscPackage (Project p) =
  case runExcept (readProp "set" p.projectFile >>= readString) of
    Right _ -> true
    _       -> false

-- | Use the provided project file, or if it is Nothing, try to find a project file
-- | path in this or any parent directory, with Bower taking precedence over psc-package.
getProjectFile :: Maybe String -> Aff String
getProjectFile = maybe search pure
  where
  search = do
    cwd <- liftEffect Process.cwd
    mbowerFile <- findIn cwd "bower.json"
    mpscPackageFile <- findIn cwd "psc-package.json"
    case mbowerFile <|> mpscPackageFile of
      Just file -> pure file
      Nothing -> throwError <<< error $
        "No bower.json or psc-package.json found in current or parent directories. Are you in a PureScript project?"

getProject :: Options -> Aff Project
getProject args = do
  bower <- getOption "bowerFile" args
  pscPackageFlag <- getFlag "pscPackage" args
  let pscPackage = if pscPackageFlag then Just "psc-package.json" else Nothing
  getProjectFile (bower <|> pscPackage) >>= readConfig

instance decodeProject :: Decode Project where
  decode o =
    map Project $ do
      projectFile <- readProp "projectFile" o
      path        <- readProp "path" o >>= readString
      cache       <- readProp "cache" o >>= readString
      pure $ { projectFile, path, cache }
