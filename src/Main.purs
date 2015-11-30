module Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Console.Unsafe as Console
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeInterleaveEff)
import Control.Monad.Eff.Exception
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Map (insert)
import Data.Foreign (parseJSON, Foreign(), toForeign)
import Data.Foreign.Class (readProp)
import Data.Version (showVersion)
import Data.Array (head)
import Data.Foldable (elem)
import Text.Parsing.Parser (ParseError(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path as Path

import Pulp.Args as Args
import Pulp.Args.Get
import Pulp.Args.Help
import Pulp.Args.Types as Type
import Pulp.Args.Parser (parse)
import Pulp.System.FFI
import Pulp.System.Log as Log
import Pulp.System.Process (argv, exit)
import Pulp.Validate (validate)
import Pulp.Version (version)
import Pulp.Project (getProject)

import Pulp.Init as Init
import Pulp.Bower as Bower
import Pulp.Build as Build
import Pulp.Run as Run
import Pulp.Test as Test
import Pulp.Browserify as Browserify
import Pulp.Docs as Docs
import Pulp.Psci as Psci
import Pulp.Server as Server
import Pulp.Watch as Watch

globals :: Array Args.Option
globals = [
  Args.option "bowerFile" ["--bower-file", "-b"] Type.file
    "Read this bower.json file instead of autodetecting it.",
  Args.option "watch" ["--watch", "-w"] Type.flag
    "Watch source directories and re-run command if something changes.",
  Args.option "monochrome" ["--monochrome"] Type.flag
    "Don't colourise log output.",
  Args.option "then" ["--then"] Type.string
    "Run a shell command after the operation finishes. Useful with `--watch`.",
  Args.option "version" ["--version", "-v"] Type.flag
    "Show current pulp version."
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

commands :: Array Args.Command
commands = [
  Args.command "init" "Generate an example PureScript project." Init.action [
     Args.option "force" ["--force"] Type.flag
       "Overwrite any project found in the current directory."
     ],
  Args.command "dep" "Invoke Bower for package management." Bower.action [],
  Args.command "build" "Build the project." Build.action buildArgs,
  Args.command "test" "Run project tests." Test.action $ [
    Args.optionDefault "main" ["--main", "-m"] Type.string
      "Test entry point." "Test.Main",
    Args.option "testRuntime" ["--runtime", "-r"] Type.string
      "Run test script using this command instead of Node.",
    Args.optionDefault "engine" ["--engine"] Type.string
      "Run the Application on a different JavaScript engine (node, iojs)" "node"
    ] ++ buildishArgs,
  Args.command "browserify"
    "Produce a deployable bundle using Browserify." Browserify.action $ [
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
      Args.option "force" ["--force"] Type.flag
        "Force a non-incremental build by deleting the build cache."
      ] ++ buildishArgs,
  Args.command "run" "Compile and run the project." Run.action $ [
    Args.optionDefault "engine" ["--engine"] Type.string
      "Run the Application on a different JavaScript engine (node, iojs)" "node"
    ] ++ buildArgs,
  Args.command "docs" "Generate project documentation." Docs.action $ [
    Args.option "withTests" ["--with-tests", "-t"] Type.flag
      "Include tests.",
    Args.option "withDependencies" ["--with-dependencies", "--with-deps", "-d"] Type.flag
      "Include external dependencies."
    ] ++ pathArgs,
  Args.command "psci" "Launch a PureScript REPL configured for the project." Psci.action pathArgs,
  Args.command "server" "Launch a Webpack development server." Server.action $ [
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

failed :: forall e a. Error -> EffN e a
failed err = do
  Console.error $ "* ERROR: " ++ message err
  logStack err
  exit 1

foreign import logStack :: forall e. Error -> EffN e Unit

succeeded :: forall e. Unit -> EffN e Unit
succeeded _ = exit 0

main :: forall e. EffN e Unit
main = runAff failed succeeded do
  args <- parse globals commands argv
  case args of
    Left (ParseError { message: err }) ->
      handleParseError err
    Right args -> do
      runArgs args
  where
  handleParseError err = go (head argv)
    where
    -- TODO: this is kind of gross, especially that --version and --help are
    -- repeated
    go (Just x)
      | x `elem` ["--version", "-v"] = liftEff $ Console.log $ showVersion version
      | x `elem` ["--help", "-h"]    = printHelp Log.out globals commands
    go _ = do
      Log.err $ "Error: " ++ err
      printHelp Log.out globals commands

-- TODO: --then
runArgs :: forall e. Args.Args -> AffN e Unit
runArgs args = do
  if "--help" `elem` args.remainder
    then printCommandHelp Log.out globals args.command
    else do
      validate
      watch <- getFlag "watch" args.globalOpts
      if watch
        then Args.runAction Watch.action args
        else do
          args' <- addProject args
          Args.runAction args.command.action args'
  where
  -- This is really quite gross, especially with _project. Not sure exactly
  -- how to go about improving this.
  addProject args =
    if args.command.name == "init"
      then return args
      else do
        proj <- getProject args.globalOpts
        let globalOpts' = insert "_project" (Just (toForeign proj)) args.globalOpts
        return $ args { globalOpts = globalOpts' }

argsParserDiagnostics :: forall e. Args.Args -> AffN e Unit
argsParserDiagnostics opts = do
  Log.log $ "Globals: " ++ show ((map <<< map) showForeign opts.globalOpts)
  Log.log $ "Command: " ++ opts.command.name
  Log.log $ "Locals: " ++ show ((map <<< map) showForeign opts.commandOpts)
  Log.log $ "Remainder: " ++ show opts.remainder
  where
  showForeign :: Foreign -> String
  showForeign = unsafeInspect
