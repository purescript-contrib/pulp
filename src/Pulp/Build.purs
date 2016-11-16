module Pulp.Build
  ( action
  , build
  , testBuild
  , withOutputStream
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe
import Data.Map (union)
import Data.String (split, Pattern(..))
import Data.Set as Set
import Data.Foldable (fold)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (attempt)
import Node.Process as Process

import Pulp.System.FFI
import Pulp.System.Stream (write, end, WritableStream())
import Pulp.Outputter
import Pulp.System.Files as Files
import Pulp.System.Which
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (psa, psc, pscBundle)
import Pulp.Files

data BuildType = NormalBuild | TestBuild

instance eqBuildType :: Eq BuildType where
  eq NormalBuild NormalBuild = true
  eq TestBuild TestBuild     = true
  eq _ _                     = false

action :: Action
action = go NormalBuild

build :: Args -> AffN Unit
build = runAction action

testBuild :: Args -> AffN Unit
testBuild = runAction (go TestBuild)

go :: BuildType -> Action
go buildType = Action \args -> do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  cwd <- liftEff Process.cwd
  out.log $ "Building project in " <> cwd

  globs <- Set.union <$> defaultGlobs opts
                     <*> (if buildType == TestBuild
                            then testGlobs opts
                            else pure Set.empty)

  buildPath <- getOption' "buildPath" args.commandOpts
  noPsa <- getFlag "noPsa" args.commandOpts
  jobs :: Maybe Int <- getOption "jobs" args.commandOpts
  let jobsArgs = maybe [] (\j -> ["+RTS", "-N" <> show j, "-RTS"]) jobs

  let sourceGlobs = sources globs
      binArgs = ["-o", buildPath] <> jobsArgs <> args.remainder
      runPsc = psc sourceGlobs binArgs Nothing

  if noPsa
    then runPsc
    else do
      psaBin <- attempt (which "psa")
      case psaBin of
        Left _ -> runPsc
        Right _ -> do
          monochrome <- getFlag "monochrome" args.globalOpts
          dependencyPath <- getOption' "dependencyPath" args.commandOpts
          let binArgs' = binArgs <> ["--is-lib=" <> dependencyPath]
                                 <> (if monochrome
                                       then ["--monochrome"]
                                       else [])
          psa sourceGlobs binArgs' Nothing

  out.log "Build successful."

  shouldBundle <- (||) <$> getFlag "optimise" opts <*> hasOption "to" opts
  when shouldBundle (bundle args)

bundle :: Args -> AffN Unit
bundle args = do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  out.log "Bundling JavaScript..."

  main      <- getOption' "main" opts
  modules   <- parseModulesOption <$> getOption "modules" opts
  buildPath <- getOption' "buildPath" opts
  skipEntry <- getFlag "skipEntryPoint" opts

  let bundleArgs = fold
        [ ["--module=" <> main]
        , if skipEntry then [] else ["--main=" <> main]
        , map (\m -> "--module=" <> m) modules
        , args.remainder
        ]

  bundledJs <- pscBundle (outputModules buildPath) bundleArgs Nothing

  withOutputStream opts $ \out' -> do
    write out' bundledJs

  out.log "Bundled."

  where
  parseModulesOption = maybe [] (split (Pattern ","))

-- | Get a writable stream which output should be written to, based on the
-- | value of the 'to' option.
withOutputStream :: Options -> (WritableStream -> AffN Unit) -> AffN Unit
withOutputStream opts aff = do
  to <- getOption "to" opts
  case to of
    Just path -> do
      stream <- liftEff $ Files.createWriteStream path
      aff stream
      end stream
    Nothing ->
      aff Process.stdout
