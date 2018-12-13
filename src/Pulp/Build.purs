module Pulp.Build
  ( action
  , build
  , testBuild
  , runBuild
  , withOutputStream
  , checkEntryPoint
  ) where

import Data.Maybe
import Prelude
import Pulp.Args
import Pulp.Args.Get
import Pulp.Files
import Pulp.Outputter
import Pulp.System.FFI

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (jsonParser)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), either)
import Data.Foldable (fold, elem, for_)
import Data.List (fromFoldable, List(..))
import Data.Map (union)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (split, Pattern(..), joinWith, trim)
import Data.Version.Haskell (Version(..))
import Effect.Aff (Aff, apathize, attempt)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Unsafe (unsafePerformEffect)
import ExternsCheck as ExternsCheck
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import Pulp.Exec (psa, pursBuild, pursBundle)
import Pulp.Sorcery (sorcery)
import Pulp.System.Files as Files
import Pulp.System.Stream (write, end, WritableStream, stdout)
import Pulp.Utils (throw)
import Pulp.Validate (getPsaVersion, getPursVersion)

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
  noPsa <- getFlag "noPsa" args.commandOpts
  sourceMaps <- getFlag "sourceMaps" args.commandOpts
  ver <- getPursVersion out
  jobs :: Maybe Int <- getOption "jobs" args.commandOpts
  let jobsArgs = maybe [] (\j -> ["+RTS", "-N" <> show j, "-RTS"]) jobs
      sourceMapArg = case sourceMaps of
        true | ver >= Version (fromFoldable [0, 12, 0]) Nil -> [ "--codegen", "sourcemaps" ]
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

  shouldBundle <- (||) <$> getFlag "optimise" opts <*> hasOption "to" opts
  when shouldBundle (bundle args)

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
  minimumPsaVersion = Version (fromFoldable [0,5,0]) Nil

bundle :: Args -> Aff Unit
bundle args = do
  let opts = union args.globalOpts args.commandOpts
  out <- getOutputter args

  out.log "Bundling JavaScript..."

  skipEntry <- getFlag "skipEntryPoint" opts
  noCheckMain <- getFlag "noCheckMain" opts
  when (not (skipEntry || noCheckMain))
    (checkEntryPoint out opts)

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

checkEntryPoint :: Outputter -> Options -> Aff Unit
checkEntryPoint out opts = do
  buildPath     <- getOption' "buildPath" opts
  main          <- getOption' "main" opts
  checkMainType <- getOption' "checkMainType" opts

  let
    externsFile =
      joinWith Path.sep [buildPath, main, "externs.json"]

    unableToParse msg =
      throw $ "Invalid JSON in externs file " <> externsFile <> ": " <> msg

    mainTypes =
      checkMainType
        # split (Pattern ",")
        # map trim
        # Array.filter (_ /= "")
        # map ExternsCheck.FQName
        # NonEmptyArray.fromArray
        # fromMaybe ExternsCheck.defaultOptions.typeConstructors

    externsCheckOpts =
      ExternsCheck.defaultOptions
        { typeConstructors = mainTypes }

    handleReadErr err =
      if Files.isENOENT err
        then throw $ "Entry point module (" <> main <> ") not found."
        else throwError err

    onError errs = do
      let hasMain = not (ExternsCheck.NoExport `elem` errs)
      if hasMain
        then do
          out.err $ main <> ".main is not suitable as an entry point because it:"
          out.err $ ""
          for_ errs (out.err <<< (" - " <> _) <<< explainErr)
        else do
          out.err $ main <> " cannot be used as an entry point module because it"
          out.err $ "does not export a `main` value."

      out.err $ ""
      out.err $ "If you need to create a JavaScript bundle without an entry point, use"
      out.err $ "the --skip-entry-point flag."
      out.err $ ""
      when hasMain do
        out.err $ "If you are certain that " <> main <> ".main has the correct runtime"
        out.err $ "representation, use the --check-main-type or --no-check-main flags"
        out.err $ "to amend or skip this check."
        out.err $ ""
      throw $ "Failed entry point check for module " <> main

    explainErr = case _ of
      ExternsCheck.NoExport ->
        internalError "NoExport should have been handled"
      ExternsCheck.TypeMismatch minstead ->
        "is not in the allowed list of types. Expected one of: "
        <> show (NonEmptyArray.toArray mainTypes)
        <> maybe "" (\instead -> " but found: " <> unwrap instead) minstead
      ExternsCheck.Constraints cs ->
        case cs of
          [] -> internalError "empty constraints array"
          [c] -> "has a " <> unwrap c <> " constraint"
          _ -> "has " <> commaList (map unwrap cs) <> " constraints"

  res <- attempt $ FS.readTextFile UTF8 externsFile
  externsStr <- either handleReadErr pure res
  externs <- either unableToParse pure $ jsonParser externsStr
  either onError pure $ ExternsCheck.checkEntryPoint externsCheckOpts externs

-- | Render a list of strings using commas.
commaList :: Array String -> String
commaList arr =
  case Array.unsnoc arr of
    Just { init: [init'], last } ->
      init' <> " and " <> last
    Just { init, last } ->
      joinWith ", " init <> ", and " <> last
    Nothing ->
      ""

internalError :: forall a. String -> a
internalError = unsafePerformEffect <<< Exception.throw <<< ("internal error: " <> _)
