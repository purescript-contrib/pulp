module Pulp.Login
  ( action
  , tokenFilePath
  ) where

import Prelude
import Control.Monad
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class
import Control.Monad.Error.Class
import Data.Maybe
import Data.Either
import Data.Foreign (toForeign)
import Data.Foreign.Class as Foreign
import Data.String as String
import Node.Process as Process
import Node.Path as Path
import Node.FS.Aff as FS
import Node.FS.Perms
import Node.Encoding (Encoding(..))
import Network.HTTP.Affjax
import Network.HTTP.StatusCode
import Network.HTTP.RequestHeader
import Network.HTTP.MimeType

import Pulp.System.FFI
import Pulp.System.Read as Read
import Pulp.Outputter
import Pulp.Args

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
  r <- affjax $ defaultRequest
    { url = "https://api.github.com/user"
    , headers = [ Accept (MimeType "application/vnd.github.v3+json")
                , RequestHeader "Authorization" ("token " <> token)
                ]
    }

  unless (r.status == StatusCode 200) $
    throwError (error case r.status of
      StatusCode 401 ->
        "Your token was not accepted (401 Unauthorized)."
      StatusCode other ->
        "Something went wrong (HTTP " <> show other <> ").")

  case Foreign.readProp "login" r.response of
    Right login' ->
      out.write ("Successfully authenticated as " <> login' <> ".\n")
    Left err ->
      throwError (error ("Unexpected response from GitHub API: " <> show err))

writeTokenFile :: String -> AffN Unit
writeTokenFile token = do
  filepath <- tokenFilePath
  mkdirIfNotExist (Path.dirname filepath)
  FS.writeTextFile UTF8 filepath token
  FS.chmod filepath (mkPerms (read + write) none none)

mkdirIfNotExist :: String -> AffN Unit
mkdirIfNotExist dirname = do
  catchError (FS.mkdir dirname) \(err :: Error) ->
    let code = Foreign.readProp "code" (toForeign err)
    in case code of
      Right "EEXIST" ->
        pure unit
      _ ->
        throwError err

tokenFilePath :: AffN String
tokenFilePath =
  (<>) <$> getHome <*> pure "/.pulp/github-oauth-token"

getHome :: AffN String
getHome = do
  home <- liftEff (Process.lookupEnv "HOME")
  case home of
    Just h ->
      pure h
    Nothing ->
      throwError (error "The HOME environment variable is not set.")
