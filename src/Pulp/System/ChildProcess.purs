module Pulp.System.ChildProcess
  ( ChildProcess()
  , spawn
  , wait
  , StdIOBehaviour(..)
  , StdIOOptions()
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))
import qualified Data.Map as Map
import Data.StrMap (StrMap())
import qualified Data.List as List
import Data.Function
import Control.Monad.Eff.Class (liftEff)
import Unsafe.Coerce (unsafeCoerce)

import Pulp.System.FFI
import Pulp.System.Process (getEnvironment)
import Pulp.System.Stream

type ChildProcess =
  { stdin  :: NodeStream String
  , stdout :: NodeStream String
  , stderr :: NodeStream String
  }

spawn :: forall e. String -> Array String -> StrMap String -> StdIOOptions -> AffN e ChildProcess
spawn cmd args env stdio = runNode $ runFn5 spawn' cmd args env (toActualStdIOOptions stdio)

foreign import spawn' :: Fn5 String
                             (Array String)
                             (StrMap String)
                             ActualStdIOOptions
                             (Callback ChildProcess)
                             Unit

wait :: forall e. ChildProcess -> AffN e Int
wait child = runNode $ runFn2 wait' child

foreign import wait' :: Fn2 ChildProcess (Callback Int) Unit

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
