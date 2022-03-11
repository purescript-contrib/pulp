module Pulp.Run where

import Prelude

import Data.Array (fold)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace, replaceAll)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import Pulp.Args (Action(..))
import Pulp.Args.Get (getOption')
import Pulp.Build as Build
import Pulp.Exec (exec)
import Pulp.Outputter (Outputter, getOutputter)
import Pulp.System.Files (openTemp)
import Pulp.Validate (dropPreRelBuildMeta, getPursVersion)
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
  exec runtime ([scriptFilePath] <> args.remainder) (Just env)

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

makeRunnableScript :: { out :: Outputter, buildPath :: String, prefix :: String, moduleName :: String } -> Aff String
makeRunnableScript { out, buildPath, prefix, moduleName } = do
  psVer <- getPursVersion out
  fullPath' <- liftEffect $ Path.resolve [] buildPath
  let
    fullPath = replaceAll (Pattern "\\") (Replacement "/") fullPath'
    makeEntry =
      if (dropPreRelBuildMeta psVer) < psVersions.v0_15_0 then
        makeCjsEntry
      else
        makeEsEntry fullPath
  src <- liftEffect $ Buffer.fromString (makeEntry moduleName) UTF8

  info <- openTemp { prefix, suffix: ".js" }
  _ <- FS.fdAppend info.fd src
  _ <- FS.fdClose info.fd
  pure info.path

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
