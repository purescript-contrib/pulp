
module Pulp.Server
  ( action
  ) where

import Prelude
import Control.Monad (when)
import Data.Maybe
import Data.Map as Map
import Data.String as String
import Data.Set as Set
import Data.Foreign (toForeign, Foreign())
import Data.String.Regex (regex, noFlags)
import Data.Function
import Control.Monad.Eff.Class (liftEff)
import Node.Path as Path
import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))

import Pulp.System.FFI
import Pulp.System.Stream as Stream
import Pulp.System.Process as Process
import Pulp.System.Log as Log
import Pulp.System.Files as Files
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (psc, pscBundle)
import Pulp.Files
import Pulp.Run (makeEntry)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts

  buildPath <- Path.resolve [] <$> getOption' "buildPath" opts
  globs <- defaultGlobs opts

  let sources' = map ("src[]=" ++) (sources globs)
  let ffis'    = map ("ffi[]=" ++) (ffis globs)

  sourceFiles <- resolveGlobs sources'

  main <- String.replace "." Path.sep <<< (++ ".purs") <$> getOption' "main" opts
  let entryPath = Path.concat ["src", ".webpack.js"]
  FS.writeTextFile UTF8 entryPath (makeEntry main)

  mconfigPath <- getOption "config" opts
  config <- case mconfigPath of
              Just path -> liftEff $ unsafeRequire $ Path.resolve [] path
              Nothing   -> liftEff $ getDefaultConfig buildPath sources' ffis'

  options <- getWebpackOptions opts

  server <- liftEff $ makeDevServer config options
  host <- getOption' "host" opts
  port <- getOption' "port" opts
  listen server host port

  Log.log $ "Server listening on http://" ++ host ++ ":" ++ show port ++ "/"

  -- TODO: watch["src"] ...

getDefaultConfig :: forall e. String -> Array String -> Array String -> EffN e Foreign
getDefaultConfig buildPath sources ffis = do
  cwd <- liftEff Process.cwd
  let nodeModulesPath = Path.resolve [__dirname] "node_modules"
  let context = Path.resolve [cwd] "src"
  pure $ defaultConfig { dir: cwd, buildPath, sources, ffis, nodeModulesPath, context }

defaultConfig :: _ -> Foreign
defaultConfig opts = toForeign $
  { 
    cache: true,
    context: opts.context,
    entry: "./.webpack.js",
    debug: true,
    devtool: "source-map",
    output: {
      path: opts.dir,
      pathinfo: true,
      filename: "app.js"
    },
    module: {
      loaders: [
        {
          test: regex "\\.purs$" noFlags,
          loader: "purs-loader?output=" ++ opts.buildPath ++
                  "&" ++ String.joinWith "&" (opts.sources ++ opts.ffis)
        }
      ]
    },
    resolve: {
      modulesDirectories: [
        "node_modules",
        "bower_components/purescript-prelude/src",
        opts.buildPath
      ],
      extensions: [ "", ".js", ".purs" ]
    },
    resolveLoader: {
      root: opts.nodeModulesPath
    }
  }

getWebpackOptions opts = do
  noInfo     <- getFlag "noInfo" opts
  quiet      <- getFlag "quiet" opts
  monochrome <- getFlag "monochrome" opts
  liftEff $ webpackOptions { noInfo, quiet, monochrome }

type WebpackOptionsArgs =
  { noInfo :: Boolean
  , quiet :: Boolean
  , monochrome :: Boolean
  }

foreign import data WebpackOptions :: *
foreign import webpackOptions :: forall e. WebpackOptionsArgs -> EffN e WebpackOptions

foreign import data DevServer :: *
foreign import makeDevServer :: forall e. Foreign -> WebpackOptions -> EffN e DevServer

foreign import listen' :: forall e. Fn4 DevServer String Int (Callback Unit) Unit

listen :: forall e. DevServer -> String -> Int -> AffN e Unit
listen server host port = runNode $ runFn4 listen' server host port

foreign import __dirname :: String
foreign import unsafeRequire :: forall e a. String -> EffN e a
