
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
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff
import Control.Monad.Aff.AVar (takeVar, putVar, makeVar)
import Unsafe.Coerce (unsafeCoerce)

import Pulp.System.Process
import Pulp.System.Stream
import Pulp.System.FFI
import Pulp.System.ChildProcess

psc = unit
pscBundle = unit

shareAll :: StdIOOptions
shareAll =
  { stdin: ShareStream stdin
  , stdout: ShareStream stdout
  , stderr: ShareStream stderr
  }

shareAllButStdout :: StdIOOptions
shareAllButStdout = shareAll { stdout = Pipe }

exec :: forall e. String -> Array String -> StrMap String -> AffN e Unit
exec cmd args env = do
  child <- spawn cmd args env shareAll
  attempt (wait child) >>= either (handleErrors cmd retry) onExit

  where
  onExit code =
    when (code > 0) $
      throwError $ error $ "Subcommand terminated with exit code " <> show code

  retry newCmd = exec newCmd args env

execQuiet :: forall e. String -> Array String -> StrMap String -> AffN e String
execQuiet cmd args env = do
  child <- spawn cmd args env shareAllButStdout
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

handleErrors :: forall e a. String -> (String -> AffN e a) -> Error -> AffN e a
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

foreign import isENOENT :: Error -> Boolean

concatStream :: forall e. NodeStream String -> AffN e String
concatStream stream = runNode $ runFn2 concatStream' stream

foreign import concatStream' :: Fn2 (NodeStream String) (Callback String) Unit
