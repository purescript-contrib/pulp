
module Pulp.Exec
  ( exec
  , execQuiet
  , execWithStdio
  , execQuietWithStderr
  , execInteractive
  , psa
  , pursBuild
  , pursBundle
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(SIGTERM, SIGINT))
import Data.String (stripSuffix, Pattern(..))
import Effect.Aff (Aff, forkAff, makeAff, throwError)
import Effect.Aff.AVar as Avar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (Object)
import Node.ChildProcess as CP
import Node.Platform (Platform(Win32))
import Node.Process as Process
import Pulp.System.Stream (concatStream, stderr, write)
import Unsafe.Coerce (unsafeCoerce)

psa :: Array String -> Array String -> Maybe (Object String) -> Aff Unit
psa = compiler "psa"

pursBuild :: Array String -> Array String -> Maybe (Object String) -> Aff Unit
pursBuild deps args = compiler "purs" deps (["compile"] <> args)

compiler :: String -> Array String -> Array String -> Maybe (Object String) -> Aff Unit
compiler name deps args env =
  execWithStdio inheritButOutToErr name (args <> deps) env
  where
  -- | Like Node.ChildProcess.inherit except the child process' standard output
  -- | is sent to Pulp's standard error.
  inheritButOutToErr = map Just
    [ CP.ShareStream (unsafeCoerce Process.stdin)
    , CP.ShareStream (unsafeCoerce Process.stderr)
    , CP.ShareStream (unsafeCoerce Process.stderr)
    ]

pursBundle :: Array String -> Array String -> Maybe (Object String) -> Aff String
pursBundle files args env =
  execQuiet "purs" (["bundle"] <> files <> args) env

-- | Start a child process asynchronously, with the given command line
-- | arguments and environment, and wait for it to exit.
-- | On a non-zero exit code, throw an error.
-- |
-- | If the executable was not found and we are on Windows, retry with ".cmd"
-- | appended.
-- |
-- | Stdout, stdin, and stderr of the child process are shared with the pulp
-- | process (that is, data on stdin from pulp is relayed to the child process,
-- | and any stdout and stderr from the child process are relayed back out by
-- | pulp, which usually means they will immediately appear in the terminal).
exec :: String -> Array String -> Maybe (Object String) -> Aff Unit
exec = execWithStdio CP.inherit

-- | Like exec, but allows you to supply your own StdIOBehaviour.
execWithStdio :: Array (Maybe CP.StdIOBehaviour) -> String -> Array String -> Maybe (Object String) -> Aff Unit
execWithStdio stdio cmd args env = do
  child <- liftEffect $ CP.spawn cmd args (def { env = env, stdio = stdio })
  wait child >>= either (handleErrors cmd retry) onExit

  where
  def = CP.defaultSpawnOptions

  onExit exit =
    case exit of
      CP.Normally 0 ->
        pure unit
      _ ->
        throwError $ error $
            "Subcommand terminated " <> showExit exit

  retry newCmd = execWithStdio stdio newCmd args env

-- | Same as exec, except instead of relaying stdout immediately, it is
-- | captured and returned as a String.
execQuiet :: String -> Array String -> Maybe (Object String) -> Aff String
execQuiet =
  execQuietWithStderr (CP.ShareStream (unsafeCoerce Process.stderr))

execQuietWithStderr :: CP.StdIOBehaviour -> String -> Array String -> Maybe (Object String) -> Aff String
execQuietWithStderr stderrBehaviour cmd args env = do
  let stdio = [ Just (CP.ShareStream (unsafeCoerce Process.stdin)) -- stdin
              , Just CP.Pipe -- stdout
              , Just stderrBehaviour -- stderr
              ]
  child <- liftEffect $ CP.spawn cmd args (def { env = env, stdio = stdio })
  outVar <- Avar.empty
  _ <- forkAff (concatStream (CP.stdout child) >>= \x -> Avar.put x outVar)
  wait child >>= either (handleErrors cmd retry) (onExit outVar)

  where
  def = CP.defaultSpawnOptions

  onExit outVar exit =
    Avar.take outVar >>= \childOut ->
      case exit of
        CP.Normally 0 ->
          pure childOut
        _ -> do
          write stderr childOut
          throwError $ error $ "Subcommand terminated " <> showExit exit

  retry newCmd = execQuietWithStderr stderrBehaviour newCmd args env

-- | A version of `exec` which installs signal handlers to make sure that the
-- | signals SIGINT and SIGTERM are relayed to the child process, if received.
execInteractive :: String -> Array String -> Maybe (Object String) -> Aff Unit
execInteractive cmd args env = do
  child <- liftEffect $ CP.spawn cmd args (def { env = env
                                            , stdio = CP.inherit })
  liftEffect $
    for_ [SIGTERM, SIGINT] \sig ->
      Process.onSignal sig
        (void (CP.kill sig child))

  wait child >>= either (handleErrors cmd retry) (const (pure unit))

  where
  def = CP.defaultSpawnOptions
  retry newCmd = exec newCmd args env

-- | A slightly weird combination of `onError` and `onExit` into one.
wait :: CP.ChildProcess -> Aff (Either CP.Error CP.Exit)
wait child = makeAff \cb -> do
  let success = cb <<< Right
  CP.onExit child (success <<< Right)
  CP.onError child (success <<< Left)
  pure mempty

showExit :: CP.Exit -> String
showExit (CP.Normally x) = "with exit code " <> show x
showExit (CP.BySignal sig) = "as a result of receiving " <> show sig

handleErrors :: forall a. String -> (String -> Aff a) -> CP.Error -> Aff a
handleErrors cmd retry err
  | err.code == "ENOENT" = do
     -- On windows, if the executable wasn't found, try adding .cmd
     if Process.platform == Just Win32
       then case stripSuffix (Pattern ".cmd") cmd of
              Nothing      -> retry (cmd <> ".cmd")
              Just bareCmd -> throwError $ error $
                 "`" <> bareCmd <> "` executable not found. (nor `" <> cmd <> "`)"
       else
         throwError $ error $
           "`" <> cmd <> "` executable not found."
  | otherwise =
     throwError (CP.toStandardError err)
