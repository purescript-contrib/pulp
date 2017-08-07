module Pulp.Project
  ( Project(..)
  , getProject
  , usingPscPackage
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.Class (class Decode)

import Node.FS.Aff (exists, readTextFile)
import Node.Encoding (Encoding(UTF8))
import Node.Path as P
import Node.Process as Process

import Pulp.System.FFI (AffN)
import Pulp.System.Files (mkdirIfNotExist)
import Pulp.Args (Options)
import Pulp.Args.Get (getOption, getFlag)

newtype Project = Project
  { projectFile :: Foreign
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
      throwError (error ("Unable to parse " <> (P.basename configFilePath) <> ": " <> show err))
    Right pro -> do
      let path = P.dirname configFilePath
      let cachePath = P.resolve [path] ".pulp-cache"
      liftEff $ unsafeCoerceEff $ Process.chdir path
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
getProjectFile :: Maybe String -> AffN String
getProjectFile = maybe search pure
  where
  search = do
    cwd <- liftEff Process.cwd
    mbowerFile <- findIn cwd "bower.json"
    mpscPackageFile <- findIn cwd "psc-package.json"
    case mbowerFile <|> mpscPackageFile of
      Just file -> pure file
      Nothing -> throwError <<< error $
        "No bower.json or psc-package.json found in current or parent directories. Are you in a PureScript project?"

getProject :: Options -> AffN Project
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
