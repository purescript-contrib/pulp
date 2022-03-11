module Pulp.Build
  ( action
  , build
  , testBuild
  , runBuild
  , withOutputStream
  , shouldBundle
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Map (union)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.Version.Haskell (Version(..))
import Effect.Aff (Aff, apathize, attempt)
import Effect.Class (liftEffect)
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import Pulp.Args (Action(..), Args, Options, runAction)
import Pulp.Args.Get (getFlag, getOption, getOption', hasOption)
import Pulp.Exec (psa, pursBuild, pursBundle)
import Pulp.Files (defaultGlobs, outputModules, sources, testGlobs)
import Pulp.Outputter (getOutputter)
import Pulp.Sorcery (sorcery)
import Pulp.System.Files as Files
import Pulp.System.Stream (write, end, WritableStream, stdout)
import Pulp.Validate (dropPreRelBuildMeta, failIfUsingEsModulesPsVersion, getPsaVersion, getPursVersion)
import Pulp.Versions.PureScript (psVersions)

data BuildType = NormalBuild | TestBuild | RunBuild

derive instance eqBuildType :: Eq BuildType

action :: Action
action = go NormalBuild

build :: Args -> Aff Unit
build = runAction action

testBuild :: Args -> Aff Unit
testBuild = runAction (go TestBuild)

runBuild :: Args -> Aff Unit
runBuild = runAction (go RunBuild)

go :: BuildType -> Action
go buildType = Action \args -> do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  cwd <- liftEffect Process.cwd
  out.log $ "Building project in " <> cwd

  globs <- Set.union <$> defaultGlobs opts
                     <*> (if buildType == TestBuild
                            then testGlobs opts
                            else pure Set.empty)

  buildPath <- getOption' "buildPath" args.commandOpts
  sourceMaps <- getFlag "sourceMaps" args.commandOpts
  ver <- getPursVersion out
  jobs :: Maybe Int <- getOption "jobs" args.commandOpts
  let jobsArgs = maybe [] (\j -> ["+RTS", "-N" <> show j, "-RTS"]) jobs
      sourceMapArg = case sourceMaps of
        true | (dropPreRelBuildMeta ver) >= psVersions.v0_12_0 -> [ "--codegen", "sourcemaps" ]
        true -> ["--source-maps"]
        _ -> []
      sourceGlobs = sources globs
      extraArgs = if buildType /= RunBuild then args.remainder else []
      binArgs = ["-o", buildPath] <> sourceMapArg <> jobsArgs <> extraArgs

  usePsa <- shouldUsePsa args
  if usePsa
    then do
      monochrome <- getFlag "monochrome" args.globalOpts
      dependencyPath <- getOption' "dependencyPath" args.commandOpts
      let binArgs' = binArgs <> ["--is-lib=" <> dependencyPath]
                             <> (if monochrome
                                then ["--monochrome"]
                                else [])
      psa sourceGlobs binArgs' Nothing
    else
      pursBuild sourceGlobs binArgs Nothing

  out.log "Build successful."

  shouldBundle' <- shouldBundle args
  when shouldBundle' do
    failIfUsingEsModulesPsVersion out $ Just
      "Code path reason: you used the --optimize and/or --to flag(s)"
    bundle args

shouldBundle :: Args -> Aff Boolean
shouldBundle args = do
  let opts = union args.globalOpts args.commandOpts
  (||) <$> getFlag "optimise" opts <*> hasOption "to" opts

shouldUsePsa :: Args -> Aff Boolean
shouldUsePsa args = do
  noPsa <- getFlag "noPsa" args.commandOpts

  if noPsa
    then
      pure false
    else do
      out <- getOutputter args
      r <- attempt (getPsaVersion out)
      case r of
        Left _ ->
          pure false
        Right v ->
          pure (v >= minimumPsaVersion)

  where
  -- TODO this is actually semver
  minimumPsaVersion = Version (NEL.cons' 0 (Cons 7 (Cons 0 Nil))) Nil

bundle :: Args -> Aff Unit
bundle args = do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  out.log "Bundling JavaScript..."

  skipEntry <- getFlag "skipEntryPoint" opts
  modules <- parseModulesOption <$> getOption "modules" opts
  buildPath <- getOption' "buildPath" opts
  main <- getOption' "main" opts
  sourceMaps <- getFlag "sourceMaps" args.commandOpts
  to :: Maybe String <- getOption "to" opts

  let bundleArgs = fold
        [ ["--module=" <> main]
        , if skipEntry then [] else ["--main=" <> main]
        , map (\m -> "--module=" <> m) modules
        , if sourceMaps then ["--source-maps"] else []
        , maybe [] (\f -> ["-o", f]) to
        , args.remainder
        ]

  bundledJs <- pursBundle (outputModules buildPath) bundleArgs Nothing

  case to of
    Just to' | sourceMaps -> sorcery to'
    Just _ -> pure unit
    _ -> withOutputStream opts $ \out' -> write out' bundledJs

  out.log "Bundled."

  where
  parseModulesOption = maybe [] (split (Pattern ","))

-- | Get a writable stream which output should be written to, based on the
-- | value of the 'to' option.
withOutputStream :: Options -> (WritableStream -> Aff Unit) -> Aff Unit
withOutputStream opts aff = do
  to :: Maybe String <- getOption "to" opts
  case to of
    Just destFile ->
      do
        let dir = Path.dirname destFile
        let tmpFile = dir <> Path.sep <> "." <> Path.basename destFile
        Files.mkdirIfNotExist dir
        res <- attempt do
                stream <- liftEffect $ Files.createWriteStream tmpFile
                void $ aff stream
                void $ end stream
        case res of
          Right _ ->
            FS.rename tmpFile destFile
          Left err -> do
            void $ apathize $ FS.unlink tmpFile
            throwError err
    Nothing ->
      aff stdout
