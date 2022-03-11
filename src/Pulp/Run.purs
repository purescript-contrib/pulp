module Pulp.Run where

import Prelude

import Data.Array (fold)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Data.Version (version)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FSA
import Node.Path as Path
import Node.Process as Process
import Pulp.Args (Action(..))
import Pulp.Args.Get (getOption')
import Pulp.Build as Build
import Pulp.Exec (exec)
import Pulp.Outputter (Outputter, getOutputter)
import Pulp.System.Files (tempDir)
import Pulp.Validate (dropPreRelBuildMeta, getNodeVersion, getPursVersion)
import Pulp.Versions.PureScript (psVersions)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  Build.runBuild args

  main <- getOption' "main" opts

  buildPath <- getOption' "buildPath" opts
  runtime <- getOption' "runtime" opts
  scriptFilePath <- makeRunnableScript { out, buildPath, prefix: "pulp-run", moduleName: main }
  env <- setupEnv buildPath
  nodeFlags <- getNodeFlags out runtime
  exec runtime (nodeFlags <> [scriptFilePath] <> args.remainder) (Just env)

-- | Given a build path, create an environment that is just like this process'
-- | environment, except with NODE_PATH set up for commands like `pulp run`.
setupEnv :: String -> Aff (Object String)
setupEnv buildPath = do
  env <- liftEffect Process.getEnv
  path <- liftEffect (Path.resolve [] buildPath)
  pure $ Object.alter (prependPath path)
                      "NODE_PATH"
                      env

prependPath :: String -> Maybe String -> Maybe String
prependPath newPath paths =
  Just $ case paths of
    Nothing -> newPath
    Just p  -> newPath <> Path.delimiter <> p

-- | Escape a string for insertion into a JS string literal.
jsEscape :: String -> String
jsEscape =
  replace (Pattern "'") (Replacement "\\'") <<<
  replace (Pattern "\\") (Replacement "\\\\")

-- | Returns an empty array or `[ "--experimental-modules" ]`
-- | if using a version of PureScript that outputs ES modules
-- | on a Node runtime with a version < `13.0.0`.
getNodeFlags :: Outputter -> String -> Aff (Array String)
getNodeFlags out runtime
  | runtime == "node" = do
      nodeVer <- getNodeVersion
      psVer <- getPursVersion out
      let
        usingEsModules = (dropPreRelBuildMeta psVer) >= psVersions.v0_15_0
        nodeNeedsFlag = nodeVer < (version 13 0 0 Nil Nil)
      pure if usingEsModules && nodeNeedsFlag then [ "--experimental-modules" ] else []
  | otherwise = pure []

makeRunnableScript :: { out :: Outputter, buildPath :: String, prefix :: String, moduleName :: String } -> Aff String
makeRunnableScript { out, buildPath, prefix, moduleName } = do
  psVer <- getPursVersion out
  fullPath' <- liftEffect $ Path.resolve [] buildPath
  let
    fullPath = replaceAll (Pattern "\\") (Replacement "/") fullPath'
    { makeEntry, writePackageJsonFile } =
      if (dropPreRelBuildMeta psVer) < psVersions.v0_15_0 then
        { makeEntry: makeCjsEntry, writePackageJsonFile: false }
      else
        { makeEntry: makeEsEntry fullPath, writePackageJsonFile: true }
  src <- liftEffect $ Buffer.fromString (makeEntry moduleName) UTF8

  parentDir <- tempDir { prefix, suffix: ".js" }
  let
    scriptFile = Path.concat [ parentDir, "index.js" ]
    packageJson = Path.concat [ parentDir, "package.json" ]
  FSA.writeFile scriptFile src
  when writePackageJsonFile do
    FSA.writeTextFile UTF8 packageJson """{"type": "module"}"""
  pure scriptFile

-- | Construct a JS string to be used as an entry point from a module name.
makeCjsEntry :: String -> String
makeCjsEntry main = "require('" <> jsEscape main <> "').main();\n"

makeEsEntry :: String -> String -> String
makeEsEntry buildPath main = fold
  [ "import { main } from 'file://"
  , buildPath
  , "/"
  , jsEscape main
  , "/index.js'; main();"
  ]
