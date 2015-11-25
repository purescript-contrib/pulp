module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Eff
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception
import Data.Either (Either(..), either)
import Data.Foreign (parseJSON, Foreign())
import Data.Foreign.Class (readProp)
import Text.Parsing.Parser (ParseError(..))
import Node.FS (FS())
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import qualified Node.Path as Path

import qualified Pulp.Args as Args
import Pulp.Args.Help
import qualified Pulp.Args.Types as Type
import Pulp.Args.Parser (parse)
import Pulp.System.FFI
import qualified Pulp.System.Log as Log
import Pulp.System.Process (argv, exit)
import Pulp.Validate (validate)

globals :: Array Args.Option
globals = [
  Args.option "bowerFile" ["--bower-file", "-b"] Type.file
    "Read this bower.json file instead of autodetecting it.",
  Args.option "watch" ["--watch", "-w"] Type.flag
    "Watch source directories and re-run command if something changes.",
  Args.option "monochrome" ["--monochrome"] Type.flag
    "Don't colourise log output.",
  Args.option "then" ["--then"] Type.string
    "Run a shell command after the operation finishes. Useful with `--watch`."
  ]

defaultDependencyPath :: String
defaultDependencyPath =
  unsafePerformEff (catchException (const (pure "bower_components")) readFromBowerRc)
  where
  readFromBowerRc = do
    json <- readTextFile UTF8 ".bowerrc"
    case parseJSON json >>= readProp "directory" of
      Right dir -> pure dir
      Left err  -> throwException (error (show err))

-- | Options for any command requiring paths
pathArgs :: Array Args.Option
pathArgs = [
  Args.option "includePaths" ["--include", "-I"] Type.directories
    ("Additional directories for PureScript source files, separated by `" ++ Path.delimiter ++ "`."),
  Args.optionDefault "srcPath" ["--src-path"] Type.directory
    "Directory for PureScript source files." "src",
  Args.optionDefault "testPath" ["--test-path"] Type.directory
    "Directory for PureScript test files." "test",
  Args.optionDefault "dependencyPath" ["--dependency-path"] Type.directory
    "Directory for PureScript dependency files." defaultDependencyPath
  ]

-- | Options common to 'build', 'test', and 'browserify'
buildishArgs :: Array Args.Option
buildishArgs = [
  Args.optionDefault "buildPath" ["--build-path", "-o"] Type.string
    "Path for compiler output." "./output",
  Args.option "optimise" ["--optimise", "-O"] Type.flag
    "Perform dead code elimination.",
  Args.option "force" ["--force"] Type.flag
    "Force a build even if no source files have changed."
  ] ++ pathArgs

buildArgs :: Array Args.Option
buildArgs = [
  Args.optionDefault "main" ["--main", "-m"] Type.string
    "Application's entry point." "Main",
  Args.option "to" ["--to", "-t"] Type.string
    "Output file name (stdout if not specified).",
  Args.option "modules" ["--modules"] Type.string
    "Additional modules to be included in the output bundle (comma-separated list)."
  ] ++ buildishArgs

nop :: Args.Action
nop _ = return unit

commands :: Array Args.Command
commands = [
  Args.command "init" "Generate an example PureScript project." nop [
     Args.option "force" ["--force"] Type.flag
       "Overwrite any project found in the current directory."
     ],
  Args.command "dep" "Invoke Bower for package management." nop [],
  Args.command "build" "Build the project." nop buildArgs,
  Args.command "test" "Run project tests." nop $ [
    Args.optionDefault "main" ["--main", "-m"] Type.string
      "Test entry point." "Test.Main",
    Args.option "testRuntime" ["--runtime", "-r"] Type.string
      "Run test script using this command instead of Node.",
    Args.optionDefault "engine" ["--engine"] Type.string
      "Run the Application on a different JavaScript engine (node, iojs)" "node"
    ] ++ buildishArgs,
  Args.command "browserify"
    "Produce a deployable bundle using Browserify." nop $ [
      Args.option "to" ["--to", "-t"] Type.string
        "Output file name for bundle (stdout if not specified).",
      Args.optionDefault "main" ["--main", "-m"] Type.string
        "Application's entry point." "Main",
      Args.option "transform" ["--transform"] Type.string
        "Apply a Browserify transform.",
      Args.option "sourceMap" ["--source-map"] Type.string
        "Generate source maps.",
      Args.option "skipEntryPoint" ["--skip-entry-point"] Type.flag
        "Don't add code to automatically invoke Main.",
      Args.option "skipCompile" ["--skip-compile"] Type.flag
        "Don't run `pulp build` before browserifying.",
      Args.option "force" ["--force"] Type.flag
        "Force a non-incremental build by deleting the build cache."
      ] ++ buildishArgs,
  Args.command "run" "Compile and run the project." nop $ [
    Args.optionDefault "engine" ["--engine"] Type.string
      "Run the Application on a different JavaScript engine (node, iojs)" "node"
    ] ++ buildArgs,
  Args.command "docs" "Generate project documentation." nop $ [
    Args.option "withTests" ["--with-tests", "-t"] Type.flag
      "Include tests.",
    Args.option "withDeps" ["--with-deps", "-d"] Type.flag
      "Include external dependencies."
    ] ++ pathArgs,
  Args.command "psci" "Launch a PureScript REPL configured for the project." nop pathArgs,
  Args.command "server" "Launch a Webpack development server." nop $ [
      Args.optionDefault "main" ["--main", "-m"] Type.string
        "Application's entry point." "Main",
      Args.option "config" ["--config", "-c"] Type.file
        "Override the default Webpack config.",
      Args.optionDefault "port" ["--port", "-p"] Type.int
        "Port number to listen on." "1337",
      Args.optionDefault "host" ["--host"] Type.string
        "IP address to bind the server to." "localhost",
      Args.option "noInfo" ["--no-info"] Type.flag
        "Display no info to the console, only warnings and errors.",
      Args.option "quiet" ["--quiet", "-q"] Type.flag
        "Display nothing to the console when rebuilding."
    ] ++ buildishArgs
  ]

failed :: forall e a. Error -> EffN (console :: CONSOLE | e) a
failed err = do
  log $ "ERROR: " ++ show err
  exit 1

succeeded :: forall e. Unit -> EffN e Unit
succeeded _ = exit 0

main :: forall e. Eff (avar :: AVAR, console :: CONSOLE, node :: Node, fs :: FS | e) Unit
main = runAff failed succeeded do
  opts <- parse globals commands argv
  case opts of
    Left (ParseError { message: err }) -> do
      Log.err $ "Error: " ++ err
      printHelp Log.out globals commands
    Right opts -> do
      validate
      Log.log $ "Globals: " ++ show ((map <<< map) showForeign opts.globalOpts)
      Log.log $ "Command: " ++ opts.command.name
      Log.log $ "Locals: " ++ show ((map <<< map) showForeign opts.commandOpts)
      Log.log $ "Remainder: " ++ show opts.remainder
  where
  showForeign :: Foreign -> String
  showForeign = unsafeInspect

foreign import unsafeInspect :: forall a. a -> String
