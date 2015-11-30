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
  { stdin  :: NodeStream String
  , stdout :: NodeStream String
  , stderr :: NodeStream String
  , pid    :: Int
  }

spawn :: forall e. String -> Array String -> Nullable (StrMap String) -> StdIOOptions -> EffN e ChildProcess
spawn cmd args env stdio = runFn4 spawn' cmd args env (toActualStdIOOptions stdio)

foreign import spawn' :: forall e.
  Fn4 String
      (Array String)
      (Nullable (StrMap String))
      ActualStdIOOptions
      (EffN e ChildProcess)

-- | Fork a child Node.js process using the current module and the provided
-- | argv (command line arguments).
foreign import fork :: forall e. Array String -> EffN e ChildProcess

wait :: forall e. ChildProcess -> AffN e Int
wait child = runNode $ runFn2 wait' child

foreign import wait' :: Fn2 ChildProcess (Callback Int) Unit

foreign import treeKill :: forall e. Int -> String -> EffN e Unit

data StdIOBehaviour
  = Pipe
  | Ignore
  | FileDescriptor Int
  | ShareStream (NodeStream String)

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
