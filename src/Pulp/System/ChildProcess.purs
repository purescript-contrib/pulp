module Pulp.System.ChildProcess
  ( ChildProcess()
  , spawn
  , fork
  , wait
  , treeKill
  , StdIOBehaviour(..)
  , StdIOOptions()
  ) where

import Prelude
import Data.StrMap (StrMap())
import Data.Nullable (Nullable())
import Data.Function
import Unsafe.Coerce (unsafeCoerce)

import Pulp.System.FFI
import Pulp.System.Stream

type ChildProcess =
  { stdin  :: WritableStream
  , stdout :: ReadableStream
  , stderr :: ReadableStream
  , pid    :: Int
  }

spawn :: String -> Array String -> Nullable (StrMap String) -> StdIOOptions -> EffN ChildProcess
spawn cmd args env stdio = runFn4 spawn' cmd args env (toActualStdIOOptions stdio)

foreign import spawn' ::
  Fn4 String
      (Array String)
      (Nullable (StrMap String))
      ActualStdIOOptions
      (EffN ChildProcess)

-- | Fork a child Node.js process using the current module and the provided
-- | argv (command line arguments).
foreign import fork :: Array String -> EffN ChildProcess

wait :: ChildProcess -> AffN Int
wait child = runNode $ runFn2 wait' child

foreign import wait' :: Fn2 ChildProcess (Callback Int) Unit

foreign import treeKill :: Int -> String -> EffN Unit

data StdIOBehaviour
  = Pipe
  | Ignore
  | FileDescriptor Int
  | ShareStream AnyStream

foreign import data ActualStdIOBehaviour :: *

toActualStdIOBehaviour :: StdIOBehaviour -> ActualStdIOBehaviour
toActualStdIOBehaviour b = case b of
  Pipe               -> f "pipe"
  Ignore             -> f "ignore"
  FileDescriptor x   -> f x
  ShareStream stream -> f stream
  where
  f :: forall a. a -> ActualStdIOBehaviour
  f = unsafeCoerce

type StdIOOptions =
  { stdin  :: StdIOBehaviour
  , stdout :: StdIOBehaviour
  , stderr :: StdIOBehaviour
  }

defaultStdIOOptions :: StdIOOptions
defaultStdIOOptions = { stdin: Pipe, stdout: Pipe, stderr: Pipe }

type ActualStdIOOptions = Array ActualStdIOBehaviour

toActualStdIOOptions :: StdIOOptions -> ActualStdIOOptions
toActualStdIOOptions opts =
 map toActualStdIOBehaviour [opts.stdin, opts.stdout, opts.stderr]
