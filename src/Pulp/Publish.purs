module Pulp.Publish ( action, resolutionsFile, BowerJson, parseJsonFile ) where

import Prelude

import Control.MonadPlus (guard)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, or)
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Version (Version)
import Data.Version as Version
import Effect.Aff (Aff, attempt, throwError)
import Effect.Class (liftEffect)
import Foreign (renderForeignError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.FS.Aff as FS
import Node.HTTP.Client as HTTP
import Node.Path as Path
import Node.Stream (pipe)
import Pulp.Args (Action(..), Args)
import Pulp.Args.Get (getFlag, getOption')
import Pulp.Exec (exec, execQuiet)
import Pulp.Git (getVersionFromGitTag, requireCleanGitWorkingTree)
import Pulp.Login (tokenFilePath)
import Pulp.Outputter (Outputter, getOutputter)
import Pulp.System.Files (isENOENT, openTemp)
import Pulp.System.HTTP (httpRequest)
import Pulp.System.Read as Read
import Pulp.System.Stream (concatStream, concatStreamToBuffer, createGzip, streamFromString)
import Pulp.Utils (orErr, throw)
import Pulp.Validate (dropPreRelBuildMeta, getPursVersion)
import Pulp.Versions.PureScript (psVersions)
import Simple.JSON as SimpleJSON

-- TODO:
-- * Check that the 'origin' remote matches with bower.json
-- * Better handling for the situation where the person running 'pulp publish'
--   doesn't actually own the repo.

action :: Action
action = Action \args -> do
  out <- getOutputter args
  out.debug "Checking bower project"
  checkBowerProject

  out.debug "Checking clean git working tree"
  requireCleanGitWorkingTree
  out.debug "Getting auth token"
  authToken <- readTokenFile
  out.debug "Parsing bower.json file"
  manifest :: BowerJson <- parseJsonFile "bower.json"
  out.debug $ "Parsed manifest:\n" <> show manifest

  out.debug "Getting resolutions file..."
  resolutionsPath <- resolutionsFile manifest args
  content <- readTextFile UTF8 resolutionsPath
  out.debug $ "Resolutions file:\n" <> content
  gzippedJson <- pursPublish resolutionsPath >>= gzip

  out.debug "Getting repo url"
  repoUrl <- map _.url manifest.repository # orErr "'repository' key not present in bower.json"
  out.debug "Verifying that repo is registered under url"
  checkRegistered out manifest.name repoUrl

  out.debug "Getting version"
  Tuple tagStr tagVersion <- getVersion
  confirm ("Publishing " <> manifest.name <> " at v" <> Version.showVersion tagVersion <> ". Is this ok?")

  noPush <- getFlag "noPush" args.commandOpts
  unless noPush do
    remote <- getOption' "pushTo" args.commandOpts
    confirmRun out "git" ["push", remote, "HEAD", "refs/tags/" <> tagStr]

  out.log "Uploading documentation to Pursuit..."
  uploadPursuitDocs out authToken gzippedJson

  out.log "Done."
  out.log ("You can view your package's documentation at: " <>
           pursuitUrl manifest.name tagVersion)

  where
  getVersion =
    getVersionFromGitTag
    >>= maybe (throw (
              "Internal error: No version could be extracted from the git tags"
              <> " in this repository. This should not have happened. Please"
              <> " report this: https://github.com/bodil/pulp/issues/new"))
          pure

checkBowerProject :: Aff Unit
checkBowerProject = do
  bower <- FS.exists "bower.json"
  if bower then pure unit
    else throw ("For the time being, libraries should be installable with Bower"
             <> " before being submitted to Pursuit. Please create a "
             <> " bower.json file first.")

checkRegistered :: Outputter -> String -> String -> Aff Unit
checkRegistered out pkgName repoUrl = do
  out.write "Checking your package is registered in purescript/registry... "
  bowerPkgs <- get "bower-packages.json" >>= parseJsonText "registry bower-packages.json"
  newPkgs <- get "new-packages.json" >>= parseJsonText "registry new-packages.json"
  case Object.lookup pkgName (Object.union bowerPkgs newPkgs) of
    Just repoUrl' -> do
      if (packageUrlIsEqual repoUrl repoUrl')
        then out.write "ok\n"
        else do
          out.write "\n"
          out.err $
            "A package with the name "
            <> pkgName
            <> " already exists in the registry, but the repository urls did not match."
          out.err "Repository url in your bower.json file:"
          out.err $ "  " <> repoUrl
          out.err "Repository url in the registry:"
          out.err $ "  " <> repoUrl'
          out.err "Please make sure these urls match."
          throw "Package repository url mismatch"
    Nothing -> do
      out.write "\n"
      out.err $
        "No package with the name "
        <> pkgName
        <> " exists in the registry."
      out.err $
        "Please register your package by sending a PR to purescript/registry first, adding your package to `new-packages.json`"
      throw "Package not registered"

  where
  get :: String -> Aff String
  get filepath = do
    let
      reqOptions = fold
        [ HTTP.method := "GET"
        , HTTP.protocol := "https:"
        , HTTP.hostname := "raw.githubusercontent.com"
        , HTTP.path := ("/purescript/registry/master/" <> filepath)
        ]
    res <- httpRequest reqOptions Nothing
    case HTTP.statusCode res of
      200 ->
        concatStream (HTTP.responseAsStream res)
      other -> do
        let msg = "Unable to fetch file " <> filepath <> " from purescript/registry"
        out.err msg
        out.err ("HTTP " <> show other <> " " <> HTTP.statusMessage res)
        out.err =<< concatStream (HTTP.responseAsStream res)
        throw msg

-- | Like normal string equality, except also allow cases where one is the same
-- | as the other except for a trailing ".git".
packageUrlIsEqual :: String -> String -> Boolean
packageUrlIsEqual a b =
  or
    [ a == b
    , a <> ".git" == b
    , a == b <> ".git"
    ]

gzip :: String -> Aff Buffer
gzip str = do
  inputStream <- liftEffect $ streamFromString str
  gzipStream <- liftEffect createGzip
  _ <- liftEffect $ inputStream `pipe` gzipStream
  concatStreamToBuffer gzipStream

-- Format for actual bower.json files, written by project maintainers. This
-- type synonym only contains the fields we care about.
type BowerJson =
  { name :: String
  , dependencies :: Maybe (Object String)
  , devDependencies :: Maybe (Object String)
  , repository ::
      Maybe { url :: String
            , type :: String
            }
  }

-- Format for .bower.json files written automatically by Bower inside
-- subdirectories of bower_components. This type synonym only contains the
-- fields we care about for extracting the necessary information for passing on
-- to `purs publish`.
type InstalledBowerJson =
  { name :: String
  , version :: String
  , _resolution ::
      { type :: String
      }
  }

-- | Create a resolutions file, using the new format where the installed
-- | version of `purs` is recent enough to be able to understand it, and using
-- | the legacy format otherwise. Returns the created file path.
resolutionsFile :: BowerJson -> Args -> Aff String
resolutionsFile manifest args = do
  out <- getOutputter args
  ver <- getPursVersion out
  resolutionsData <-
    if (dropPreRelBuildMeta ver) >= psVersions.v0_12_4
      then do
        let hasDependencies =
              (maybe false (not <<< Object.isEmpty) $ manifest.dependencies)
              || (maybe false (not <<< Object.isEmpty) $ manifest.devDependencies)
        dependencyPath <- getOption' "dependencyPath" args.commandOpts
        getResolutions hasDependencies dependencyPath
      else
        getResolutionsLegacy
  writeResolutionsFile resolutionsData

-- Obtain resolutions information for a Bower project as a string containing
-- JSON, using the new format.
getResolutions :: Boolean -> String -> Aff String
getResolutions hasDeps dependencyPath = do
  serializeResolutions <$>
    if hasDeps
      then getResolutionsBower dependencyPath
      else pure []

-- Obtain resolutions information for a Bower project. If a dependency has been
-- installed in a non-standard way, e.g. via a particular branch or commit
-- rather than a published version, the `version` field for that package in the
-- result will be Nothing.
getResolutionsBower ::
  String ->
  Aff
    (Array
      { packageName :: String
      , version :: Maybe String
      , path :: String
      })
getResolutionsBower dependencyPath = do
  dependencyDirs <- FS.readdir dependencyPath
  flip parTraverse dependencyDirs \dir -> do
    pkgInfo :: InstalledBowerJson <-
      parseJsonFile (Path.concat [dependencyPath, dir, ".bower.json"])
    let
      packageName =
        pkgInfo.name
      version =
        guard (pkgInfo._resolution."type" == "version")
        *> Just pkgInfo.version
      path =
        dependencyPath <> Path.sep <> dir
    pure
      { packageName
      , version
      , path
      }

serializeResolutions ::
  Array
    { packageName :: String
    , version :: Maybe String
    , path :: String
    } ->
  String
serializeResolutions rs =
  let
    toKeyValuePair { packageName, version, path } =
      Tuple packageName { version, path }
    obj =
      Object.fromFoldable (map toKeyValuePair rs)
  in
    SimpleJSON.writeJSON obj

getResolutionsLegacy :: Aff String
getResolutionsLegacy =
  execQuiet "bower" ["list", "--json", "--offline"] Nothing

writeResolutionsFile :: String -> Aff String
writeResolutionsFile resolutionsContents = do
  info <- openTemp { prefix: "pulp-publish", suffix: ".json" }
  _ <- FS.fdAppend info.fd =<< liftEffect (Buffer.fromString resolutionsContents UTF8)
  _ <- FS.fdClose info.fd
  pure info.path

pursPublish :: String -> Aff String
pursPublish resolutionsPath =
  execQuiet
    "purs"
    ["publish", "--manifest", "bower.json", "--resolutions", resolutionsPath]
    Nothing

confirmRun :: Outputter -> String -> Array String -> Aff Unit
confirmRun out cmd args = do
  out.log "About to execute:"
  out.write ("> " <> cmd <> " " <> String.joinWith " " args <> "\n")
  confirm "Ok?"
  exec cmd args Nothing

confirm :: String -> Aff Unit
confirm q = do
  answer <- Read.read { prompt: q <> " [y/n] ", silent: false }
  case String.trim (String.toLower answer) of
    "y" ->
      pure unit
    _ ->
      throw "Aborted"

readTokenFile :: Aff String
readTokenFile = do
  path <- tokenFilePath
  r <- attempt (FS.readTextFile UTF8 path)
  case r of
    Right token ->
      pure token
    Left err | isENOENT err ->
      throw "Pursuit authentication token not found. Try running `pulp login` first."
    Left err ->
      throwError err

pursuitUrl :: String -> Version -> String
pursuitUrl name vers =
  "https://pursuit.purescript.org/packages/" <> name <> "/" <> Version.showVersion vers

uploadPursuitDocs :: Outputter -> String -> Buffer -> Aff Unit
uploadPursuitDocs out authToken gzippedJson = do
  res <- httpRequest reqOptions (Just gzippedJson)
  case HTTP.statusCode res of
    201 ->
      pure unit
    other -> do
      out.err =<< concatStream (HTTP.responseAsStream res)
      out.err $ HTTP.statusMessage res
      out.bolded $ "This command may fail with a 400 error from Pursuit on the first run. Try running it a second time before debugging further."
      throw ("Expected an HTTP 201 response from Pursuit, got: " <> show other)

  where
  headers =
    HTTP.RequestHeaders (Object.fromFoldable
      [ "Accept" /\ "application/json"
      , "Authorization" /\ ("token " <> authToken)
      , "Content-Encoding" /\ "gzip"
      ])

  reqOptions = fold
    [ HTTP.method := "POST"
    , HTTP.protocol := "https:"
    , HTTP.hostname := "pursuit.purescript.org"
    , HTTP.path := "/packages"
    , HTTP.headers := headers
    ]

-- | Read a file containing JSON text and parse it, or throw an error.
parseJsonFile :: forall a. SimpleJSON.ReadForeign a => String -> Aff a
parseJsonFile filePath = do
  json <- FS.readTextFile UTF8 filePath
  parseJsonText ("file " <> filePath) json

-- | Parse some JSON, or throw an error.
parseJsonText :: forall a. SimpleJSON.ReadForeign a => String -> String -> Aff a
parseJsonText source json = do
  case SimpleJSON.readJSON json of
    Left errs ->
      throw ("Error while decoding " <> source <> ":\n"
        <> String.joinWith "; " (Array.fromFoldable (map renderForeignError errs)))
    Right x ->
      pure x
