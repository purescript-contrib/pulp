module Pulp.Build
  ( action
  , build
  , testBuild
  , withOutputStream
  , checkEntryPoint
  ) where

import Prelude
import Data.Either (Either(..), either)
import Data.Array as Array
import Data.Maybe
import Data.Map (union)
import Data.String (split, Pattern(..), joinWith)
import Data.Set as Set
import Data.Foldable (fold, elem, for_)
import Data.Newtype (unwrap)
import Data.List (fromFoldable, List(..))
import Data.Version.Haskell (Version(..))
import Data.Argonaut (jsonParser)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Aff (attempt, apathize)
import Node.Process as Process
import Node.Path as Path
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import ExternsCheck as ExternsCheck

import Pulp.System.FFI
import Pulp.System.Stream (write, end, WritableStream, stdout)
import Pulp.Outputter
import Pulp.System.Files as Files
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (psa, pursBuild, pursBundle)
import Pulp.Files
import Pulp.Validate (getPsaVersion)
import Pulp.Utils (throw)

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

shouldUsePsa :: Args -> AffN Boolean
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

bundle :: Args -> AffN Unit
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

  let bundleArgs = fold
        [ ["--module=" <> main]
        , if skipEntry then [] else ["--main=" <> main]
        , map (\m -> "--module=" <> m) modules
        , args.remainder
        ]

  bundledJs <- pursBundle (outputModules buildPath) bundleArgs Nothing

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
    Just destFile -> do
      let dir = Path.dirname destFile
      let tmpFile = dir <> Path.sep <> "." <> Path.basename destFile
      Files.mkdirIfNotExist dir
      res <- attempt do
               stream <- liftEff $ Files.createWriteStream tmpFile
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

checkEntryPoint :: Outputter -> Options -> AffN Unit
checkEntryPoint out opts = do
  buildPath <- getOption' "buildPath" opts
  main      <- getOption' "main" opts
  tyConstr  <- getOption' "checkMainType" opts

  let
    externsFile =
      joinWith Path.sep [buildPath, main, "externs.json"]

    unableToParse msg =
      throw $ "Invalid JSON in externs file " <> externsFile <> ": " <> msg

    externsCheckOpts =
      ExternsCheck.defaultOptions
        { typeConstructor = ExternsCheck.FQName tyConstr }

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
        "is not of type " <> tyConstr
        <> maybe "" (\instead -> " (is instead " <> unwrap instead <> ")") minstead
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
internalError = unsafePerformEff <<< Exception.throw <<< ("internal error: " <> _)
