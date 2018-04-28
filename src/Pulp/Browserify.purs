
module Pulp.Browserify where

import Data.Foldable
import Data.Function.Uncurried
import Data.Maybe
import Prelude
import Pulp.Args
import Pulp.Args.Get
import Pulp.Files
import Pulp.Outputter
import Pulp.Project
import Pulp.Sorcery
import Pulp.System.FFI
import Pulp.System.Files

import Control.Monad.Aff (apathize)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, foldJsonArray, foldJsonObject, foldJsonString, fromArray, fromObject, fromString, jsonEmptyObject, jsonNull, jsonParser)
import Data.Argonaut.Core (stringify)
import Data.Either (either)
import Data.Map as Map
import Data.Nullable (Nullable, toNullable)
import Data.StrMap (update)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (unlink, writeTextFile, readTextFile)
import Node.Path as Path
import Node.Process as Process
import Pulp.Build as Build
import Pulp.Exec (pursBundle)
import Pulp.Run (makeEntry, jsEscape)
import Pulp.System.Stream (WritableStream)

action :: Action
action = Action \args -> do
  out <- getOutputter args

  cwd <- liftEff Process.cwd
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
  else makeEntry main

makeOptExport :: String -> String
makeOptExport main = "module.exports = PS[\"" <> jsEscape main <> "\"];\n"

buildForBrowserify :: Args -> AffN Unit
buildForBrowserify args = do
  skip <- getFlag "skipCompile" args.commandOpts
  when (not skip) do
    let munge = Map.delete "to" >>> Map.delete "optimise"
    Build.build $ args { commandOpts = munge args.commandOpts, remainder = [] }

shouldSkipEntryPoint :: Options -> AffN Boolean
shouldSkipEntryPoint opts = do
  skipEntryPoint <- getFlag "skipEntryPoint" opts
  standalone :: Maybe String <- getOption "standalone" opts
  pure (skipEntryPoint || isJust standalone)

shouldSkipMainCheck :: Options -> AffN Boolean
shouldSkipMainCheck opts = do
  noCheckMain <- getFlag "noCheckMain" opts
  skipEntryPoint <- shouldSkipEntryPoint opts
  pure (noCheckMain || skipEntryPoint)

optimising :: Action
optimising = Action \args -> do
  out <- getOutputter args

  let opts = Map.union args.globalOpts args.commandOpts

  globs     <- defaultGlobs opts
  buildPath <- getOption' "buildPath" opts
  main      <- getOption' "main" opts
  transform <- getOption "transform" opts
  standalone <- getOption "standalone" opts
  sourceMaps <- getFlag "sourceMaps" opts
  toOpt <- getOption "to" opts

  unlessM (shouldSkipMainCheck opts)
    (Build.checkEntryPoint out opts)

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
    writeTextFile UTF8 mapFile (updateSourceMapPaths (Path.dirname mapFile) smText)

  out.log "Browserifying..."

  liftEff $ setupNodePath buildPath

  Build.withOutputStream opts $ \out' -> do
    browserifyBundle
      { basedir: Path.resolve [] buildPath
      , src: bundledJs <> if isJust standalone then makeOptExport main else ""
      , transform: toNullable transform
      , standalone: toNullable standalone
      , out: out'
      , debug: sourceMaps
      , outDir: maybe buildPath (Path.resolve [ buildPath ] <<< Path.dirname) toOpt
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
  liftEff $ setupNodePath buildPath

  force       <- getFlag "force" opts
  Project pro <- getOption' "_project" opts
  let cachePath = Path.resolve [pro.cache] "browserify.json"
  when force
    (apathize $ unlink cachePath)

  transform <- getOption "transform" opts
  standalone <- getOption "standalone" opts
  main <- getOption' "main" opts
  sourceMaps <- getFlag "sourceMaps" opts

  unlessM (shouldSkipMainCheck opts)
    (Build.checkEntryPoint out opts)

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
    browserifyIncBundle
      { basedir: buildPath
      , cacheFile: cachePath
      , path: path
      , transform: toNullable transform
      , standalone: toNullable standalone
      , out: out'
      , debug: sourceMaps
      , outDir: maybe buildPath (Path.resolve [ buildPath ] <<< Path.dirname) toOpt
      }
  case toOpt of
    Just to | sourceMaps -> sorcery to
    _ -> pure unit

-- | Given the build path, modify this process' NODE_PATH environment variable
-- | for browserify.
setupNodePath :: String -> EffN Unit
setupNodePath buildPath = do
  nodePath <- Process.lookupEnv "NODE_PATH"
  let buildPath' = Path.resolve [] buildPath
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

foreign import browserifyBundle' :: Fn2 BrowserifyOptions
                                        (Callback Unit)
                                        Unit

browserifyBundle :: BrowserifyOptions -> AffN Unit
browserifyBundle opts = runNode $ runFn2 browserifyBundle' opts

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

foreign import browserifyIncBundle' :: Fn2 BrowserifyIncOptions
                                          (Callback Unit)
                                          Unit

browserifyIncBundle :: BrowserifyIncOptions -> AffN Unit
browserifyIncBundle opts = runNode $ runFn2 browserifyIncBundle' opts

updateSourceMapPaths :: String -> String -> String
updateSourceMapPaths basePath text =
  either (const text) 
    (stringify <<< foldJsonObject jsonEmptyObject (fromObject <<< update resolveFiles "sources"))
    (jsonParser text)
  where
    resolveFiles :: Json -> Maybe Json
    resolveFiles = foldJsonArray Nothing (Just <<< fromArray <<< map resolveFile)

    resolveFile :: Json -> Json
    resolveFile = foldJsonString jsonNull (fromString <<< Path.resolve [ basePath ])
