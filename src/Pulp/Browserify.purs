
module Pulp.Browserify where

import Prelude
import Control.Monad.Aff (apathize)
import Control.Monad.Eff.Class (liftEff)
import Data.Function.Uncurried
import Data.Maybe
import Data.Map as Map
import Data.Nullable (Nullable(), toNullable)
import Node.Path as Path
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (unlink, writeTextFile)
import Node.Process as Process

import Pulp.System.FFI
import Pulp.System.Stream (WritableStream())
import Pulp.Outputter
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (pscBundle)
import Pulp.Files
import Pulp.Build as Build
import Pulp.Run (makeEntry, jsEscape)
import Pulp.Project

action :: Action
action = Action \args -> do
  out <- getOutputter args

  cwd <- liftEff Process.cwd
  out.log $ "Browserifying project in " <> cwd

  optimise <- getFlag "optimise" args.commandOpts
  let act = if optimise then optimising else incremental

  runAction act args

  out.log "Browserified."

makeExport :: String -> Boolean -> String
makeExport main export =
  if export
  then "module.exports = require(\"" <> jsEscape main <> "\");\n"
  else makeEntry main

makeOptExport :: String -> String
makeOptExport main = "module.exports = PS[\"" <> jsEscape main <> "\"];\n"

optimising :: Action
optimising = Action \args -> do
  out <- getOutputter args

  let munge = Map.delete "to" >>> Map.delete "optimise"
  Build.build $ args { commandOpts = munge args.commandOpts }

  let opts = Map.union args.globalOpts args.commandOpts

  globs     <- defaultGlobs opts
  buildPath <- getOption' "buildPath" opts
  main      <- getOption' "main" opts
  transform <- getOption "transform" opts
  standalone <- getOption "standalone" opts
  skipEntryPoint' <- getFlag "skipEntryPoint" opts
  let skipEntryPoint = skipEntryPoint' || isJust standalone

  bundledJs <- pscBundle (outputModules buildPath)
                         (["--module=" <> main]
                          <> if skipEntryPoint
                             then []
                             else ["--main=" <> main]
                          <> args.remainder)
                         Nothing

  out.log "Browserifying..."

  liftEff $ setupNodePath buildPath

  Build.withOutputStream opts $ \out' -> do
    browserifyBundle
      { basedir: Path.resolve [] buildPath
      , src: bundledJs <> if isJust standalone then makeOptExport main else ""
      , transform: toNullable transform
      , standalone: toNullable standalone
      , out: out'
      }

incremental :: Action
incremental = Action \args -> do
  out <- getOutputter args

  let munge = Map.delete "to"
  Build.build $ args { commandOpts = munge args.commandOpts }

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
  skipEntryPoint' <- getFlag "skipEntryPoint" opts
  let skipEntryPoint = skipEntryPoint' && isNothing standalone
  main <- getOption' "main" opts
  path <- if skipEntryPoint
            then
              pure $ Path.concat [buildPath, main]
            else do
              let entryJs = makeExport main $ isJust standalone
              let entryPath = Path.concat [buildPath, "browserify.js"]
              writeTextFile UTF8 entryPath entryJs
              pure entryPath

  Build.withOutputStream opts $ \out' -> do
    browserifyIncBundle
      { basedir: buildPath
      , cacheFile: cachePath
      , path: path
      , transform: toNullable transform
      , standalone: toNullable standalone
      , out: out'
      }

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
  }

foreign import browserifyIncBundle' :: Fn2 BrowserifyIncOptions
                                          (Callback Unit)
                                          Unit

browserifyIncBundle :: BrowserifyIncOptions -> AffN Unit
browserifyIncBundle opts = runNode $ runFn2 browserifyIncBundle' opts
