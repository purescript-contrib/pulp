module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (head, drop)
import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.List (List(Nil))
import Data.Map (insert)
import Data.Maybe (Maybe(..))
import Data.Version (Version, showVersion, version)
import Effect (Effect)
import Effect.Aff (Aff, attempt, runAff, throwError)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error, catchException, error, message, throwException)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, unsafeToForeign, readString)
import Foreign.Index (readProp)
import Foreign.JSON (parseJSON)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Node.Process as Process
import Pulp.Args as Args
import Pulp.Args.Get (getFlag, getOption)
import Pulp.Args.Help (printCommandHelp, printHelp)
import Pulp.Args.Parser (parse)
import Pulp.Args.Types as Type
import Pulp.Browserify as Browserify
import Pulp.Build as Build
import Pulp.BumpVersion as BumpVersion
import Pulp.Docs as Docs
import Pulp.Init as Init
import Pulp.Login as Login
import Pulp.Outputter (getOutputter, makeOutputter)
import Pulp.Project (getProject)
import Pulp.Publish as Publish
import Pulp.Repl as Repl
import Pulp.Run as Run
import Pulp.Server as Server
import Pulp.Shell as Shell
import Pulp.System.FFI (unsafeInspect)
import Pulp.Test as Test
import Pulp.Validate (getNodeVersion, validate)
import Pulp.Version (printVersion)
import Pulp.Watch as Watch
import Text.Parsing.Parser (parseErrorMessage)

globals :: Array Args.Option
globals = [
  Args.option "bowerFile" ["--bower-file", "-b"] Type.file
    "Read this bower.json file instead of autodetecting it.",
  Args.option "pscPackage" ["--psc-package"] Type.flag
    "Use psc-package for package management.",
  Args.option "debug" ["--debug"] Type.flag
    "Enable debug logging",
  Args.option "watch" ["--watch", "-w"] Type.flag
    "Watch source directories and re-run command if something changes.",
  Args.option "monochrome" ["--monochrome"] Type.flag
    "Don't colourise log output.",
  Args.option "before" ["--before"] Type.string
    "Run a shell command before the operation begins. Useful with `--watch`, eg. `--watch --before clear`.",
  Args.option "then" ["--then"] Type.string
    "Run a shell command after the operation finishes successfully. Useful with `--watch`, eg. `--watch --then 'say Done'`",
  Args.option "else" ["--else"] Type.string
    "Run a shell command if an operation failed. Useful with `--watch`, eg. `--watch --then 'say Done' --else 'say Failed'`",
  Args.option "version" ["--version", "-v"] Type.flag
    "Show current pulp version."
  ]

defaultDependencyPath :: String
defaultDependencyPath =
  unsafePerformEffect (catchException (const (pure "bower_components")) readFromBowerRc)
  where
  readFromBowerRc = do
    json <- readTextFile UTF8 ".bowerrc"
    case runExcept (parseJSON json >>= readProp "directory" >>= readString) of
      Right dir -> pure dir
      Left err  -> throwException (error (show err))

dependencyPathOption :: Args.Option
dependencyPathOption =
  Args.optionDefault "dependencyPath" ["--dependency-path"] Type.directory
    "Directory for PureScript dependency files." defaultDependencyPath

-- | Options for any command requiring paths
pathArgs :: Array Args.Option
pathArgs = [
  Args.optionDefault "includePaths" ["--include", "-I"] Type.directories
    ("Additional directories for PureScript source files, separated by `" <> Path.delimiter <> "`.")
    ([] :: Array String),
  Args.optionDefault "srcPath" ["--src-path"] Type.directory
    "Directory for PureScript source files." "src",
  Args.optionDefault "testPath" ["--test-path"] Type.directory
    "Directory for PureScript test files." "test",
  dependencyPathOption
  ]

buildPath :: Args.Option
buildPath =
  Args.optionDefault "buildPath" ["--build-path", "-o"] Type.string
    "Path for compiler output." "./output"

-- | Options common to 'build', 'test', and 'browserify'
buildishArgs :: Array Args.Option
buildishArgs = [
  buildPath,
  Args.option "noPsa" ["--no-psa"] Type.flag
    "Do not attempt to use the psa frontend instead of purs compile"
  ] <> pathArgs

runArgs :: Array Args.Option
runArgs = [
  Args.optionDefault "main" ["--main", "-m"] Type.string
    "Module to be used as the application's entry point." "Main",
  Args.option "jobs" ["--jobs", "-j"] Type.int
    "Tell purs to use the specified number of cores."
  ] <> buildishArgs

buildArgs :: Array Args.Option
buildArgs = [
  Args.option "to" ["--to", "-t"] Type.string
    "Output file name (stdout if not specified).",
  Args.option "optimise" ["--optimise", "-O"] Type.flag
    "Perform dead code elimination.",
  Args.option "skipEntryPoint" ["--skip-entry-point"] Type.flag
    "Don't add code to automatically invoke Main.",
  Args.option "sourceMaps" ["--source-maps"] Type.flag
    "Generate source maps"
  ] <> runArgs

-- TODO: This is possibly just a temporary separation from buildArgs; at the
--       moment, the browserify action doesn't support this option, but it's
--       definitely in the realm of possibility.
moduleArgs :: Array Args.Option
moduleArgs = [
  Args.option "modules" ["--modules"] Type.string
    "Additional modules to be included in the output bundle (comma-separated list)."
  ]

remainderToPurs :: Maybe String
remainderToPurs = Just "Passthrough options are sent to `purs compile`."

remainderToTest :: Maybe String
remainderToTest =
  Just ("Passthrough options are sent to the test program. " <>
    "This can be useful for only running one particular test, for instance.")

remainderToBundle :: Maybe String
remainderToBundle =
  Just "Passthrough options are sent to `purs bundle`."

remainderToProgram :: Maybe String
remainderToProgram =
  Just "Passthrough options are sent to your program."

remainderToDocs :: Maybe String
remainderToDocs =
  Just "Passthrough options are sent to `purs docs`."

remainderToRepl :: Maybe String
remainderToRepl =
  Just "Passthrough options are sent to `purs repl`."

commands :: Array Args.Command
commands = [
  Args.command "init" "Generate an example PureScript project." Nothing Init.action [
     Args.option "force" ["--force"] Type.flag
       "Overwrite any project found in the current directory.",
     Args.option "withEff" ["--with-eff"] Type.flag
       "Generate project using Eff, regardless of the detected compiler version.",
     Args.option "withEffect" ["--with-effect"] Type.flag
       "Generate project using Effect, regardless of the detected compiler version."
     ],
  Args.command "build" "Build the project." remainderToPurs Build.action $
    buildArgs <> moduleArgs,
  Args.command "test" "Run project tests." remainderToTest Test.action $ [
    Args.optionDefault "main" ["--main", "-m"] Type.string
      "Test entry point." "Test.Main",
    Args.optionDefault "runtime" ["--runtime", "-r"] Type.string
      "Run test script using this command instead of Node." "node"
    ] <> buildishArgs,
  Args.command "browserify"
    "Produce a deployable bundle using Browserify." remainderToBundle Browserify.action $ [
      Args.option "transform" ["--transform"] Type.string
        "Apply a Browserify transform.",
      Args.option "force" ["--force"] Type.flag
        "Force a non-incremental build by deleting the build cache.",
      Args.option "standalone" ["--standalone"] Type.string
        "Output a UMD bundle with the given external module name.",
      Args.option "skipCompile" ["--skip-compile"] Type.flag
        "Assume PureScript code has already been compiled. Useful for when you want to pass options to purs."
      ] <> buildArgs,
  Args.command "run" "Compile and run the project." remainderToProgram Run.action $ [
    Args.optionDefault "runtime" ["--runtime", "-r"] Type.string
      "Run the program using this command instead of Node." "node"
    ] <> runArgs,
  Args.command "docs" "Generate project documentation." remainderToDocs Docs.action $ [
    buildPath,
    Args.option "withTests" ["--with-tests", "-t"] Type.flag
      "Include tests."
    ] <> pathArgs,
  Args.commandWithAlias "repl"
    "Launch a PureScript REPL configured for the project." remainderToRepl
    Repl.action pathArgs
    ["psci"],
  Args.command "server" "Launch a development server." Nothing Server.action $ [
      Args.optionDefault "main" ["--main", "-m"] Type.string
        "Application's entry point." "Main",
      Args.optionDefault "port" ["--port", "-p"] Type.int
        "Port number to listen on." 1337,
      Args.optionDefault "host" ["--host"] Type.string
        "IP address to bind the server to." "localhost",
      Args.option "quiet" ["--quiet", "-q"] Type.flag
        "Display nothing to the console when rebuilding."
    ] <> buildishArgs,
  Args.command "login" "Obtain and store a token for uploading packages to Pursuit." Nothing Login.action [],
  Args.commandWithArgs "version" "Bump and tag a new version in preparation for release." Nothing BumpVersion.action [ dependencyPathOption ]
    [ Args.argument "bump" Type.versionBump "How to bump the version. Acceptable values: 'major', 'minor', 'patch', or any specific version. If omitted, Pulp will prompt you for a version." false
    ],
  Args.command "publish" "Publish a previously tagged version to Bower and Pursuit." Nothing Publish.action [
      Args.optionDefault "pushTo" ["--push-to"] Type.string
        "The Git remote to push commits and tags to." "origin",
      Args.option "noPush" ["--no-push"] Type.flag
        "Skip pushing commits or tags to any remote.",
      dependencyPathOption
    ]
  ]

failed :: forall a. Error -> Effect a
failed err = do
  Console.error $ "* ERROR: " <> message err
  -- logStack err
  Process.exit 1

foreign import logStack :: Error -> Effect Unit

succeeded :: Unit -> Effect Unit
succeeded = const (pure unit)

main :: Effect Unit
main = void $ runAff (either failed succeeded) do
                requireNodeAtLeast (version 12 0 0 Nil Nil)
                argv <- drop 2 <$> liftEffect Process.argv
                args <- parse globals commands argv
                case args of
                  Left err ->
                    handleParseError (head argv) (parseErrorMessage err)
                  Right (Left (Args.Help command)) ->
                    printCommandHelp out globals command
                  Right (Right args') ->
                    runWithArgs args'
  where
  handleParseError (Just x) _
    -- TODO: this is kind of gross, especially that --version and --help are
    -- repeated
    | x `elem` ["--version", "-v"] = printVersion
    | x `elem` ["--help", "-h"]    = printHelp out globals commands

  handleParseError _ err = do
    out.err $ "Error: " <> err
    printHelp out globals commands
    liftEffect $ Process.exit 1

  out = makeOutputter false false

runWithArgs :: Args.Args -> Aff Unit
runWithArgs args = do
  out <- getOutputter args
  _ <- validate out
  watch <- getFlag "watch" args.globalOpts
  args' <- addProject args
  if watch && args.command.name /= "server"
    then
      Args.runAction Watch.action args'
    else do
      runShellForOption "before" args'.globalOpts out
      result <- attempt $ Args.runAction args.command.action args'
      case result of
        Left e  -> do
          runShellForOption "else" args'.globalOpts out
          liftEffect $ throwException e
        Right _ ->
          runShellForOption "then" args'.globalOpts out
  where
  noProject = ["init", "login"]

  -- This is really quite gross, especially with _project. Not sure exactly
  -- how to go about improving this.
  addProject as =
    if as.command.name `elem` noProject
      then pure as
      else do
        proj <- getProject as.globalOpts
        let globalOpts' = insert "_project" (Just (unsafeToForeign proj)) as.globalOpts
        pure $ as { globalOpts = globalOpts' }

  runShellForOption option opts out = do
    triggerCommand <- getOption option opts
    case triggerCommand of
      Just cmd -> Shell.shell out cmd
      Nothing  -> pure unit

requireNodeAtLeast :: Version -> Aff Unit
requireNodeAtLeast minimum = do
  actual <- getNodeVersion
  when (actual < minimum) do
    throwError $ error $
      "Your node.js version is too old " <>
        "(required: " <> showVersion minimum <>
        ", actual: " <> showVersion actual <> ")"

argsParserDiagnostics :: Args.Args -> Aff Unit
argsParserDiagnostics opts = do
  let out = makeOutputter false true
  out.log $ "Globals: " <> show ((map <<< map) showForeign opts.globalOpts)
  out.log $ "Command: " <> opts.command.name
  out.log $ "Locals: " <> show ((map <<< map) showForeign opts.commandOpts)
  out.log $ "Remainder: " <> show opts.remainder
  where
  showForeign :: Foreign -> String
  showForeign = unsafeInspect
