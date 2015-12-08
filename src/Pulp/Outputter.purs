module Pulp.Outputter
  ( Outputter()
  , getOutputter
  , makeOutputter
  ) where

import Prelude

import Pulp.System.Ansi
import Pulp.System.FFI
import Pulp.System.Process (stderr)
import Pulp.System.Stream (write)
import Pulp.System.SupportsColor as Color
import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get

type Outputter e =
  { log :: String -> AffN e Unit
  , err :: String -> AffN e Unit
  , write  :: String -> AffN e Unit
  , bolded :: String -> AffN e Unit
  , monochrome :: Boolean
  }

-- | Get an outputter, with monochrome based on the command line arguments.
getOutputter :: forall e. Args -> AffN e (Outputter e)
getOutputter args =
  makeOutputter <$> getFlag "monochrome" args.globalOpts

-- | Get an outputter. The argument represents "monochrome"; if true is
-- | supplied, the returned logger will never use color. Otherwise, whether or
-- | not colour is used depends on the "supports-color" module. Note that the
-- | `monochrome` attribute of the returned outputter might not necessarily
-- | be the same as the argument supplied.
makeOutputter :: forall e. Boolean -> Outputter e
makeOutputter monochrome =
  if not monochrome && Color.hasBasic
    then ansiOutputter
    else monochromeOutputter

monochromeOutputter :: forall e. Outputter e
monochromeOutputter =
  { log: monobullet
  , err: monobullet
  , write: write stderr
  , bolded: write stderr
  , monochrome: true
  }
  where
  monobullet text = write stderr ("* " ++ text ++ "\n")

ansiOutputter :: forall e. Outputter e
ansiOutputter =
  { log: bullet ansiOut "green"
  , err: bullet ansiOut "red"
  , write: write ansiOut
  , bolded: bolded ansiOut
  , monochrome: false
  }

ansiOut :: Ansi
ansiOut = ansi stderr

bullet :: forall e. Ansi -> String -> String -> AffN e Unit
bullet out colour text = do
  col out colour
  bold out
  write out "* "
  reset out
  write out $ text ++ "\n"
