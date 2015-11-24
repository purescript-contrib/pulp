module Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Eff
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Exception
import Data.Either (Either(..))
import Text.Parsing.Parser (ParseError(..))
import Node.FS (FS())

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

-- | Options common to 'build', 'test', and 'browserify'
buildTestBrowserifyArgs :: Array Args.Option
buildTestBrowserifyArgs = [
  Args.optionDefault "buildPath" ["--build-path", "-o"] Type.string
    "Path for compiler output." "./output",
  Args.option "includePaths" ["--include", "-I"] Type.string
    "Additional directories for PureScript source files, separated by spaces."
  ]

buildArgs :: Array Args.Option
buildArgs = [
  Args.optionDefault "main" ["--main", "-m"] Type.string
    "Application's entry point." "Main",
  Args.option "optimise" ["--optimise", "-O"] Type.flag
    "Perform dead code elimination.",
  Args.option "to" ["--to", "-t"] Type.string
    "Output file name (stdout if not specified)."
  ] ++ buildTestBrowserifyArgs

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
    ] ++ buildTestBrowserifyArgs,
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
      ] ++ buildTestBrowserifyArgs,
  Args.command "run" "Compile and run the project." nop $ [
    Args.optionDefault "engine" ["--engine"] Type.string
      "Run the Application on a different JavaScript engine (node, iojs)" "node"
    ] ++ buildArgs,
  Args.command "docs" "Generate project documentation." nop [
    Args.option "withTests" ["--with-tests", "-t"] Type.flag
      "Include tests.",
    Args.option "withDeps" ["--with-deps", "-d"] Type.flag
      "Include external dependencies."
    ],
  Args.command "psci"
    "Launch a PureScript REPL configured for the project." nop []
  ]

failed :: forall e. Error -> EffN (console :: CONSOLE | e) Unit
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
      Log.log $ "Globals: " ++ show opts.globalOpts
      Log.log $ "Command: " ++ opts.command.name
      Log.log $ "Locals: " ++ show opts.commandOpts
      Log.log $ "Remainder: " ++ show opts.remainder
