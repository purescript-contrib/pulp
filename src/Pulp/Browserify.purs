
module Pulp.Browserify where

import Prelude
import Control.Monad (when)
import Data.Function
import Data.Maybe
import Data.Map as Map
import Data.Nullable (Nullable(), toNullable)
import Control.Monad.Eff.Class (liftEff)
import Node.Path as Path
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (unlink, writeTextFile)
import Node.Path as Path

import Pulp.System.FFI
import Pulp.System.Stream as Stream
import Pulp.System.Process as Process
import Pulp.Outputter
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (pscBundle)
import Pulp.Files
import Pulp.Build as Build
import Pulp.Run (makeEntry)
import Pulp.Project

action :: Action
action = Action \args -> do
  out <- getOutputter args

  cwd <- liftEff Process.cwd
  out.log $ "Browserifying project in " ++ cwd

  optimise <- getFlag "optimise" args.commandOpts
  let act = if optimise then optimising else incremental

  runAction act args

  out.log "Browserified."

optimising :: Action
optimising = Action \args -> do
  out <- getOutputter args

  let munge = Map.delete "to" >>> Map.delete "optimise"
  Build.build $ args { commandOpts = munge args.commandOpts }

  let opts = Map.union args.globalOpts args.commandOpts

  globs     <- defaultGlobs opts
  buildPath <- getOption' "buildPath" opts
  main      <- getOption' "main" opts

  bundledJs <- pscBundle (outputModules buildPath)
                         (["--module=" ++ main, "--main=" ++ main]
                          ++ args.remainder)
                         Nothing

  out.log "Browserifying..."

  liftEff $ setupNodePath buildPath

  transform <- getOption "transform" opts
  out' <- Build.getOutputStream opts

  browserifyBundle
    { basedir: Path.resolve [] buildPath
    , src: bundledJs
    , transform: toNullable transform
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
    (unlink cachePath)

  skipEntryPoint <- getFlag "skipEntryPoint" opts
  main <- getOption' "main" opts
  path <- if skipEntryPoint
            then
              pure $ Path.concat [buildPath, main]
            else do
              let entryJs = makeEntry main
              let entryPath = Path.concat [buildPath, "browserify.js"]
              writeTextFile UTF8 entryPath entryJs
              pure entryPath

  transform <- getOption "transform" opts
  out' <- Build.getOutputStream opts

  browserifyIncBundle
    { basedir: buildPath
    , cacheFile: cachePath
    , path: path
    , transform: toNullable transform
    , out: out'
    }

-- | Given the build path, modify this process' NODE_PATH environment variable
-- | for browserify.
setupNodePath :: forall e. String -> EffN e Unit
setupNodePath buildPath = do
  nodePath <- Process.getEnv "NODE_PATH"
  let buildPath' = Path.resolve [] buildPath
  Process.setEnv "NODE_PATH" $
    case nodePath of
      Just nodePath -> buildPath' ++ Path.delimiter ++ nodePath
      Nothing       -> buildPath'

type BrowserifyOptions =
  { basedir   :: String
  , src       :: String
  , transform :: Nullable String
  , out       :: Stream.NodeStream String
  }

foreign import browserifyBundle' :: Fn2 BrowserifyOptions
                                        (Callback Unit)
                                        Unit

browserifyBundle :: forall e. BrowserifyOptions -> AffN e Unit
browserifyBundle opts = runNode $ runFn2 browserifyBundle' opts

type BrowserifyIncOptions =
  { basedir   :: String
  , cacheFile :: String
  , path      :: String
  , transform :: Nullable String
  , out       :: Stream.NodeStream String
  }

foreign import browserifyIncBundle' :: Fn2 BrowserifyIncOptions
                                          (Callback Unit)
                                          Unit

browserifyIncBundle :: forall e. BrowserifyIncOptions -> AffN e Unit
browserifyIncBundle opts = runNode $ runFn2 browserifyIncBundle' opts
