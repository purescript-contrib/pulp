module Main where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Array.Unsafe (head)
import Data.Either (Either(..))
import Debug.Trace

import qualified Pulp.Args as Args
import qualified Pulp.Args.Types as Type
import Pulp.Args.Parser (parse)
import Pulp.System.Process (argv, exit)

globals :: [Args.Option]
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

mainlessBuildArgs :: [Args.Option]
mainlessBuildArgs = [
  Args.optionDefault "buildPath" ["--build-path", "-o"] Type.string
    "Path for compiler output." "./output"
  ]

buildArgs :: [Args.Option]
buildArgs = [
  Args.optionDefault "main" ["--main", "-m"] Type.string
    "Application's entry point." "Main"
  ] ++ mainlessBuildArgs

nop :: Args.Action
nop _ = return unit

commands :: [Args.Command]
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
      "Run test script using this command instead of Node."
    ] ++ mainlessBuildArgs,
  Args.command "browserify"
    "Produce a deployable bundle using Browserify." nop [
      Args.option "to" ["--to", "-t"] Type.string
        "Output file name for bundle (stdout if not specified).",
      Args.option "transform" ["--transform"] Type.string
        "Apply a Browserify transform.",
      Args.option "sourceMap" ["--source-map"] Type.string
        "Generate source maps.",
      Args.option "optimise" ["--optimise", "-O"] Type.flag
        "Perform dead code elimination.",
      Args.option "skipEntryPoint" ["--skip-entry-point"] Type.flag
        "Don't add code to automatically invoke Main.",
      Args.option "skipCompile" ["--skip-compile"] Type.flag
        "Don't run `pulp build` before browserifying."
      ],
  Args.command "run" "Compile and run the project." nop buildArgs,
  Args.command "docs" "Generate project documentation." nop [
    Args.option "withTests" ["--with-tests", "-t"] Type.flag
      "Include tests.",
    Args.option "withDeps" ["--with-deps", "-d"] Type.flag
      "Include external dependencies."
    ],
  Args.command "psci"
    "Launch a PureScript REPL configured for the project." nop []
  ]

failed :: forall e. Error -> Eff (trace :: Trace | e) Unit
failed err = do
  trace $ "ERROR: " ++ show err
  exit 1

succeeded :: forall e. Unit -> Eff e Unit
succeeded _ = exit 0

log :: forall e. String -> Aff (trace :: Trace | e) Unit
log s = apathize $ liftEff' $ trace s

main = runAff failed succeeded do
  opts <- parse globals commands argv
  case opts of
    Left err -> log $ "Error: " ++ show err
    Right opts -> do
      log $ "Globals: " ++ show opts.globalOpts
      log $ "Command: " ++ opts.command.name
      log $ "Locals: " ++ show opts.commandOpts
      log $ "Remainder: " ++ show opts.remainder
