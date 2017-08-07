module Pulp.Publish ( action, resolutionsFile ) where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Error.Class
import Control.Monad.Aff
import Control.Monad.Except (runExcept)
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested ((/\))
import Data.Either
import Data.Foldable (fold)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Version (Version)
import Data.Version as Version
import Data.String as String
import Data.StrMap as StrMap
import Data.Options ((:=))
import Node.Encoding (Encoding(..))
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.FS.Aff as FS
import Node.HTTP.Client as HTTP

import Pulp.System.FFI
import Pulp.System.HTTP
import Pulp.System.Stream
import Pulp.Exec
import Pulp.Args
import Pulp.Args.Get
import Pulp.Outputter
import Pulp.System.Files
import Pulp.System.Read as Read
import Pulp.Git
import Pulp.Login (tokenFilePath)
import Pulp.Utils (throw)

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

  gzippedJson <- pursPublish >>= gzip

  Tuple tagStr tagVersion <- getVersion
  bowerJson <- readBowerJson

  name <- getBowerName bowerJson
  confirm ("Publishing " <> name <> " at v" <> Version.showVersion tagVersion <> ". Is this ok?")

  noPush <- getFlag "noPush" args.commandOpts
  unless noPush do
    remote <- getOption' "pushTo" args.commandOpts
    confirmRun out "git" ["push", remote, "HEAD", "refs/tags/" <> tagStr]

    -- Only attempt to register on Bower after a successful push, to avoid
    -- accidental squatting by non-package-owners.
    repoUrl <- getBowerRepositoryUrl bowerJson
    registerOnBowerIfNecessary out name repoUrl

  out.log "Uploading documentation to Pursuit..."
  uploadPursuitDocs out authToken gzippedJson

  out.log "Done."
  out.log ("You can view your package's documentation at: " <>
           pursuitUrl name tagVersion)

  where
  getVersion =
    getVersionFromGitTag
    >>= maybe (throw (
              "Internal error: No version could be extracted from the git tags"
              <> " in this repository. This should not have happened. Please"
              <> " report this: https://github.com/bodil/pulp/issues/new"))
          pure

checkBowerProject :: AffN Unit
checkBowerProject = do
  bower <- FS.exists "bower.json"
  if bower then pure unit
    else throw ("For the time being, libraries should be published on Bower"
             <> " before being submitted to Pursuit. Please create a "
             <> " bower.json file first.")

gzip :: String -> AffN Buffer
gzip str = do
  gzipStream <- liftEff createGzip
  write gzipStream str
  end gzipStream
  concatStreamToBuffer gzipStream

resolutionsFile :: AffN String
resolutionsFile = do
  resolutions <- execQuiet "bower" ["list", "--json", "--offline"] Nothing
  info <- openTemp { prefix: "pulp-publish", suffix: ".json" }
  _ <- FS.fdAppend info.fd =<< liftEff (Buffer.fromString resolutions UTF8)
  _ <- FS.fdClose info.fd
  pure info.path

pursPublish :: AffN String
pursPublish = do
  resolutions <- resolutionsFile
  execQuiet "purs" ["publish", "--manifest", "bower.json", "--resolutions", resolutions] Nothing

confirmRun :: Outputter -> String -> Array String -> AffN Unit
confirmRun out cmd args = do
  out.log "About to execute:"
  out.write ("> " <> cmd <> " " <> String.joinWith " " args <> "\n")
  confirm "Ok?"
  exec cmd args Nothing

confirm :: String -> AffN Unit
confirm q = do
  answer <- Read.read { prompt: q <> " [y/n] ", silent: false }
  case String.trim (String.toLower answer) of
    "y" ->
      pure unit
    _ ->
      throw "Aborted"

newtype BowerJson = BowerJson Foreign

readBowerJson :: AffN BowerJson
readBowerJson = do
  json <- FS.readTextFile UTF8 "bower.json"
  case runExcept (parseJSON json) of
    Right parsedJson ->
      pure (BowerJson parsedJson)
    Left err ->
      throw ("Unable to parse bower.json:" <> show err)

getBowerName :: BowerJson -> AffN String
getBowerName (BowerJson json) =
  case runExcept (readProp "name" json >>= readString) of
    Right name ->
      pure name
    Left err ->
      throw ("Unable to read property 'name' from bower.json:" <> show err)

getBowerRepositoryUrl :: BowerJson -> AffN String
getBowerRepositoryUrl (BowerJson json) =
  case runExcept (readProp "repository" json >>= readProp "url" >>= readString) of
    Right url ->
      pure url
    Left err ->
      throw ("Unable to read property 'repository.url' from bower.json:" <> show err)

readTokenFile :: AffN String
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

registerOnBowerIfNecessary :: Outputter -> String -> String -> AffN Unit
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

uploadPursuitDocs :: Outputter -> String -> Buffer -> AffN Unit
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
    HTTP.RequestHeaders (StrMap.fromFoldable
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
