
module Pulp.Exec
  ( exec
  , execQuiet
  , psc
  , pscBundle
  ) where

import Prelude
import Data.Either (either)
import Data.Function
import Data.String (stripSuffix)
import Data.StrMap (StrMap())
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Nullable (toNullable)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff
import Control.Monad.Aff.AVar (takeVar, putVar, makeVar)

import Pulp.System.Process
import Pulp.System.Stream
import Pulp.System.FFI
import Pulp.System.ChildProcess
import Pulp.System.Files

psc :: Array String -> Array String -> Array String -> Maybe (StrMap String) -> AffN String
psc deps ffi args env =
  let allArgs = args <> deps <> (Array.concatMap (\path -> ["--ffi", path]) ffi)
  in  execQuiet "psc" allArgs env

pscBundle :: Array String -> Array String -> Maybe (StrMap String) -> AffN String
pscBundle files args env =
  execQuiet "psc-bundle" (files <> args) env

shareAll :: StdIOOptions
shareAll =
  { stdin: ShareStream stdin
  , stdout: ShareStream stdout
  , stderr: ShareStream stderr
  }

shareAllButStdout :: StdIOOptions
shareAllButStdout = shareAll { stdout = Pipe }

-- | Start a child process asynchronously, with the given command line
-- | arguments and environment, and wait for it to exit.
-- | On a non-zero exit code, throw an error.
--
-- | If the executable was not found and we are on Windows, retry with ".cmd"
-- | appended.
-- |
-- | Stdout, stdin, and stderr of the child process are shared with the pulp
-- | process (that is, data on stdin from pulp is relayed to the child process,
-- | and any stdout and stderr from the child process are relayed back out by
-- | pulp, which usually means they will immediately appear in the terminal).
exec :: String -> Array String -> Maybe (StrMap String) -> AffN Unit
exec cmd args env = do
  child <- liftEff $ spawn cmd args (toNullable env) shareAll
  attempt (wait child) >>= either (handleErrors cmd retry) onExit

  where
  onExit code =
    when (code > 0) $
      throwError $ error $ "Subcommand terminated with exit code " <> show code

  retry newCmd = exec newCmd args env

-- | Same as exec, except instead of relaying stdout immediately, it is
-- | captured and returned as a String.
execQuiet :: String -> Array String -> Maybe (StrMap String) -> AffN String
execQuiet cmd args env = do
  child <- liftEff $ spawn cmd args (toNullable env) shareAllButStdout
  outVar <- makeVar
  forkAff (concatStream child.stdout >>= putVar outVar)
  attempt (wait child) >>= either (handleErrors cmd retry) (onExit outVar)

  where
  onExit outVar code =
    takeVar outVar >>= \childOut ->
      if code == 0
        then return childOut
        else do
          write stderr childOut
          throwError $ error $ "Subcommand terminated with exit code " <> show code

  retry newCmd = execQuiet newCmd args env

handleErrors :: forall a. String -> (String -> AffN a) -> Error -> AffN a
handleErrors cmd retry err
  | isENOENT err = do
     platformWin32 <- ("win32" ==) <$> liftEff getPlatform
     -- On windows, if the executable wasn't found, try adding .cmd
     if platformWin32
       then case stripSuffix ".cmd" cmd of
              Nothing      -> retry (cmd <> ".cmd")
              Just bareCmd -> throwError $ error $
                 "`" <> bareCmd <> "` executable not found. (nor `" <> cmd <> "`)"
       else
         throwError $ error $
           "`" <> cmd <> "` executable not found."
  | otherwise =
     throwError err

concatStream :: NodeStream String -> AffN String
concatStream stream = runNode $ runFn2 concatStream' stream

foreign import concatStream' :: Fn2 (NodeStream String) (Callback String) Unit
