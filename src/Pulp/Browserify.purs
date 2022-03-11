
module Pulp.Browserify where

import Prelude

import Data.Argonaut (Json, caseJsonArray, caseJsonObject, caseJsonString, fromArray, fromObject, fromString, jsonEmptyObject, jsonNull, jsonParser)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, apathize)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (unlink, writeTextFile, readTextFile)
import Node.Path as Path
import Node.Process as Process
import Pulp.Args (Action(..), Args, Options, runAction)
import Pulp.Args.Get (getFlag, getOption, getOption')
import Pulp.Build as Build
import Pulp.Exec (pursBundle)
import Pulp.Files (outputModules)
import Pulp.Outputter (getOutputter)
import Pulp.Project (Project(..))
import Pulp.Run (jsEscape, makeCjsEntry)
import Pulp.Sorcery (sorcery)
import Pulp.System.FFI (Callback, runNode)
import Pulp.System.Files (openTemp)
import Pulp.System.Stream (WritableStream)
import Pulp.Validate (failIfUsingEsModulesPsVersion)

action :: Action
action = Action \args -> do
  out <- getOutputter args

  failIfUsingEsModulesPsVersion out $ Just
    "Code path reason: browserify only works on CommonJS modules"

  cwd <- liftEffect Process.cwd
  out.log $ "Browserifying project in " <> cwd

  optimise <- getFlag "optimise" args.commandOpts
  let act = if optimise then optimising else incremental

  buildForBrowserify args
  runAction act args

  out.log "Browserified."

makeExport :: String -> Boolean -> String
makeExport main export =
  if export
  then "module.exports = require(\"" <> jsEscape main <> "\");\n"
  else makeCjsEntry main

makeOptExport :: String -> String
makeOptExport main = "module.exports = PS[\"" <> jsEscape main <> "\"];\n"

buildForBrowserify :: Args -> Aff Unit
buildForBrowserify args = do
  skip <- getFlag "skipCompile" args.commandOpts
  when (not skip) do
    let munge = Map.delete "to" >>> Map.delete "optimise"
    Build.build $ args { commandOpts = munge args.commandOpts, remainder = [] }

shouldSkipEntryPoint :: Options -> Aff Boolean
shouldSkipEntryPoint opts = do
  skipEntryPoint <- getFlag "skipEntryPoint" opts
  standalone :: Maybe String <- getOption "standalone" opts
  pure (skipEntryPoint || isJust standalone)

optimising :: Action
optimising = Action \args -> do
  out <- getOutputter args

  let opts = Map.union args.globalOpts args.commandOpts

  buildPath <- getOption' "buildPath" opts
  main      <- getOption' "main" opts
  transform <- getOption "transform" opts
  standalone <- getOption "standalone" opts
  sourceMaps <- getFlag "sourceMaps" opts
  toOpt <- getOption "to" opts

  { path: tmpFilePath } <- openTemp { prefix: "pulp-browserify-bundle-", suffix: ".js" }

  skipEntryPoint <- shouldSkipEntryPoint opts
  let bundleArgs = fold
        [ ["--module=" <> main]
        , if skipEntryPoint then [] else ["--main=" <> main]
        , if sourceMaps then ["--source-maps"] else []
        , ["-o", tmpFilePath]
        , args.remainder
        ]

  _ <- pursBundle (outputModules buildPath) bundleArgs Nothing
  bundledJs <- readTextFile UTF8 tmpFilePath

  let mapFile = tmpFilePath <> ".map"
  when sourceMaps do
    smText <- readTextFile UTF8 mapFile
    path <- liftEffect $ updateSourceMapPaths (Path.dirname mapFile) smText
    writeTextFile UTF8 mapFile path

  out.log "Browserifying..."

  liftEffect $ setupNodePath buildPath

  Build.withOutputStream opts $ \out' -> do
    basedir <- liftEffect $ Path.resolve [] buildPath
    outDir <- liftEffect $ maybe (pure buildPath) (Path.resolve [ buildPath ] <<< Path.dirname) toOpt
    browserifyBundle
      { basedir
      , src: bundledJs <> if isJust standalone then makeOptExport main else ""
      , transform: toNullable transform
      , standalone: toNullable standalone
      , out: out'
      , debug: sourceMaps
      , outDir
      , tmpFilePath: tmpFilePath
      }
  case toOpt of
    Just to | sourceMaps -> do
      sorcery to
      unlink mapFile
    _ -> pure unit

incremental :: Action
incremental = Action \args -> do
  out <- getOutputter args

  let opts = Map.union args.globalOpts args.commandOpts

  out.log "Browserifying..."

  buildPath <- getOption' "buildPath" opts
  liftEffect $ setupNodePath buildPath

  force       <- getFlag "force" opts
  Project pro <- getOption' "_project" opts
  cachePath <- liftEffect $ Path.resolve [pro.cache] "browserify.json"
  when force
    (apathize $ unlink cachePath)

  transform <- getOption "transform" opts
  standalone <- getOption "standalone" opts
  main <- getOption' "main" opts
  sourceMaps <- getFlag "sourceMaps" opts

  skipEntryPoint <- shouldSkipEntryPoint opts
  path <- if skipEntryPoint
            then
              pure $ Path.concat [buildPath, main]
            else do
              let entryJs = makeExport main $ isJust standalone
              let entryPath = Path.concat [buildPath, "browserify.js"]
              writeTextFile UTF8 entryPath entryJs
              pure entryPath

  toOpt <- getOption "to" opts
  Build.withOutputStream opts $ \out' -> do
    outDir <- liftEffect $ maybe (pure buildPath) (Path.resolve [ buildPath ] <<< Path.dirname) toOpt
    browserifyIncBundle
      { basedir: buildPath
      , cacheFile: cachePath
      , path: path
      , transform: toNullable transform
      , standalone: toNullable standalone
      , out: out'
      , debug: sourceMaps
      , outDir
      }
  case toOpt of
    Just to | sourceMaps -> sorcery to
    _ -> pure unit

-- | Given the build path, modify this process' NODE_PATH environment variable
-- | for browserify.
setupNodePath :: String -> Effect Unit
setupNodePath buildPath = do
  nodePath <- Process.lookupEnv "NODE_PATH"
  buildPath' <- Path.resolve [] buildPath
  Process.setEnv "NODE_PATH" $
    case nodePath of
      Just p  -> buildPath' <> Path.delimiter <> p
      Nothing -> buildPath'

type BrowserifyOptions =
  { basedir    :: String
  , src        :: String
  , transform  :: Nullable String
  , standalone :: Nullable String
  , out        :: WritableStream
  , debug      :: Boolean
  , outDir     :: String
  , tmpFilePath:: String
  }

foreign import browserifyBundleImpl :: Fn2 BrowserifyOptions
                                        (Callback Unit)
                                        Unit

browserifyBundle :: BrowserifyOptions -> Aff Unit
browserifyBundle opts = runNode $ runFn2 browserifyBundleImpl opts

type BrowserifyIncOptions =
  { basedir    :: String
  , cacheFile  :: String
  , path       :: String
  , transform  :: Nullable String
  , standalone :: Nullable String
  , out        :: WritableStream
  , debug      :: Boolean
  , outDir     :: String
  }

foreign import browserifyIncBundleImpl :: Fn2 BrowserifyIncOptions
                                          (Callback Unit)
                                          Unit

browserifyIncBundle :: BrowserifyIncOptions -> Aff Unit
browserifyIncBundle opts = runNode $ runFn2 browserifyIncBundleImpl opts

updateSourceMapPaths :: String -> String -> Effect String
updateSourceMapPaths basePath text =
  case jsonParser text of
    Left _ -> pure text
    Right json -> do
      resolutions <- caseJsonObject (pure jsonEmptyObject) (map fromObject <<< updateWithEffect resolveFiles "sources") json
      pure (stringify resolutions)
  where
    updateWithEffect effect key map = do
      value <- maybe (pure Nothing) effect (Object.lookup key map)
      pure $ Object.update (const value) key map

    resolveFiles :: Json -> Effect (Maybe Json)
    resolveFiles = caseJsonArray (pure Nothing) (map (Just <<< fromArray) <<< traverse resolveFile)

    resolveFile :: Json -> Effect Json
    resolveFile = caseJsonString (pure jsonNull) (map fromString <<< Path.resolve [ basePath ])
