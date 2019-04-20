module Pulp.Publish ( action, resolutionsFile, BowerJson, parseJsonFile ) where

import Prelude

import Control.MonadPlus (guard)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Version (Version)
import Data.Version as Version
import Data.Version.Haskell (Version(..)) as Haskell
import Effect.Aff (Aff, attempt, throwError)
import Effect.Class (liftEffect)
import Foreign (renderForeignError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.HTTP.Client as HTTP
import Node.Path as Path
import Pulp.Args (Action(..), Args)
import Pulp.Args.Get (getFlag, getOption')
import Pulp.Exec (exec, execQuiet, execQuietWithStderr)
import Pulp.Git (getVersionFromGitTag, requireCleanGitWorkingTree)
import Pulp.Login (tokenFilePath)
import Pulp.Outputter (Outputter, getOutputter)
import Pulp.System.Files (isENOENT, openTemp)
import Pulp.System.HTTP (httpRequest)
import Pulp.System.Read as Read
import Pulp.System.Stream (concatStream, concatStreamToBuffer, createGzip, end, write)
import Pulp.Utils (orErr, throw)
import Pulp.Validate (getPursVersion)
import Simple.JSON as SimpleJSON

-- TODO:
-- * Check that the 'origin' remote matches with bower.json
-- * Better handling for the situation where the person running 'pulp publish'
--   doesn't actually own the repo.

action :: Action
action = Action \args -> do
  checkBowerProject

  out <- getOutputter args

  requireCleanGitWorkingTree
  authToken <- readTokenFile
  manifest :: BowerJson <- parseJsonFile "bower.json"

  resolutionsPath <- resolutionsFile manifest args
  gzippedJson <- pursPublish resolutionsPath >>= gzip

  Tuple tagStr tagVersion <- getVersion
  confirm ("Publishing " <> manifest.name <> " at v" <> Version.showVersion tagVersion <> ". Is this ok?")

  noPush <- getFlag "noPush" args.commandOpts
  unless noPush do
    remote <- getOption' "pushTo" args.commandOpts
    confirmRun out "git" ["push", remote, "HEAD", "refs/tags/" <> tagStr]

    -- Only attempt to register on Bower after a successful push, to avoid
    -- accidental squatting by non-package-owners.
    repoUrl <- map _.url manifest.repository # orErr "'repository' key not present in bower.json"
    registerOnBowerIfNecessary out manifest.name repoUrl

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
    else throw ("For the time being, libraries should be published on Bower"
             <> " before being submitted to Pursuit. Please create a "
             <> " bower.json file first.")

gzip :: String -> Aff Buffer
gzip str = do
  gzipStream <- liftEffect createGzip
  write gzipStream str
  end gzipStream
  concatStreamToBuffer gzipStream

-- Format for actual bower.json files, written by project maintainers. This
-- type synonym only contains the fields we care about.
type BowerJson =
  { name :: String
  , dependencies :: Maybe (Object String)
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
    if ver >= Haskell.Version (List.fromFoldable [0,12,4]) Nil
      then do
        let hasDependencies =
              maybe false (not <<< Object.isEmpty) manifest.dependencies
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

registerOnBowerIfNecessary :: Outputter -> String -> String -> Aff Unit
registerOnBowerIfNecessary out name repoUrl = do
  result <- attempt (run "bower" ["info", name, "--json"] Nothing)
  case result of
    Left _ -> do
      out.log "Registering your package on Bower..."
      confirmRun out "bower" ["register", name, repoUrl]
    Right _ ->
      -- already registered, don't need to do anything.
      pure unit
  where
  -- Run a command, sending stderr to /dev/null
  run = execQuietWithStderr CP.Ignore

uploadPursuitDocs :: Outputter -> String -> Buffer -> Aff Unit
uploadPursuitDocs out authToken gzippedJson = do
  res <- httpRequest reqOptions (Just gzippedJson)
  case HTTP.statusCode res of
    201 ->
      pure unit
    other -> do
      out.err =<< concatStream (HTTP.responseAsStream res)
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
  case SimpleJSON.readJSON json of
    Left errs ->
      throw ("Error while decoding " <> filePath <> ":\n"
        <> String.joinWith "; " (Array.fromFoldable (map renderForeignError errs)))
    Right x ->
      pure x
