module Pulp.Login
  ( action
  , tokenFilePath
  ) where

import Prelude
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class
import Control.Monad.Error.Class
import Control.Monad.Except (runExcept)
import Data.Maybe
import Data.Tuple.Nested
import Data.Either
import Data.Foldable (fold)
import Data.Foreign (readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.JSON (parseJSON)
import Data.String as String
import Data.StrMap as StrMap
import Data.Options ((:=))
import Node.Process as Process
import Node.Platform (Platform(Win32))
import Node.Path as Path
import Node.FS.Aff as FS
import Node.FS.Perms
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as HTTP

import Pulp.System.HTTP
import Pulp.System.FFI
import Pulp.System.Stream (concatStream)
import Pulp.System.Files (mkdirIfNotExist)
import Pulp.System.Read as Read
import Pulp.Outputter
import Pulp.Args
import Pulp.Version as PulpVersion

-- TODO: Obtain tokens automatically after prompting for a username and
-- password.
--
-- Unfortunately it is not easy to do this without exposing the client secret,
-- so I think we need to add a route to Pursuit itself to support this, so that
-- Pursuit sort of proxies to GitHub and adds its client secret itself.

action :: Action
action = Action \args -> do
  out <- getOutputter args

  token <- obtainTokenFromStdin out
  checkToken out token

  writeTokenFile token

obtainTokenFromStdin :: Outputter -> AffN String
obtainTokenFromStdin out = do
  out.write "Please obtain a GitHub personal access token at:\n"
  out.write "  https://github.com/settings/tokens/new\n"
  out.write "No scopes are required, so don't check any of the boxes.\n"
  out.write "\n"
  String.trim <$> Read.read
    { prompt: "After you've done that, paste it in here: "
    , silent: true
    }

checkToken :: Outputter -> String -> AffN Unit
checkToken out token = do
  res <- httpRequest reqOptions Nothing

  let statusCode = HTTP.statusCode res
  resBody <- concatStream (HTTP.responseAsStream res)
  unless (statusCode == 200) $
    throwError (error case statusCode of
      401 ->
        "Your token was not accepted (401 Unauthorized)."
      other ->
        let
          header =
            "Something went wrong (HTTP " <> show other <> " " <>
            HTTP.statusMessage res <> ")."
        in
          header <> "\n" <> resBody)

  case runExcept (parseJSON resBody >>= readProp "login" >>= readString) of
    Right login' ->
      out.write ("Successfully authenticated as " <> login' <> ".\n")
    Left err ->
      throwError (error ("Unexpected response from GitHub API: " <> show err))

  where
  reqOptions = fold
    [ HTTP.protocol := "https:"
    , HTTP.hostname := "api.github.com"
    , HTTP.path := "/user"
    , HTTP.headers := HTTP.RequestHeaders (StrMap.fromFoldable
        [ "Accept" /\ "application/vnd.github.v3+json"
        , "Authorization" /\ ("token " <> token)
        , "User-Agent" /\ ("Pulp-" <> PulpVersion.versionString)
        ])
    ]

writeTokenFile :: String -> AffN Unit
writeTokenFile token = do
  filepath <- tokenFilePath
  mkdirIfNotExist (Path.dirname filepath)
  FS.writeTextFile UTF8 filepath token
  FS.chmod filepath (mkPerms (read + write) none none)

tokenFilePath :: AffN String
tokenFilePath =
  (<>) <$> getHome <*> pure "/.pulp/github-oauth-token"

getHome :: AffN String
getHome = do
  let homeVar = if Process.platform == Just Win32 then "USERPROFILE" else "HOME"
  home <- liftEff (Process.lookupEnv homeVar)
  case home of
    Just h ->
      pure h
    Nothing ->
      throwError (error (
        "The " <> homeVar <> " environment variable is not set."))
