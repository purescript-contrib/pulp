module Pulp.Outputter
  ( Outputter()
  , getOutputter
  , makeOutputter
  ) where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground, bold)
import Effect.Aff (Aff)
import Pulp.Args (Args)
import Pulp.Args.Get (getFlag)
import Pulp.System.Stream (write, WritableStream, stderr)
import Pulp.System.SupportsColor as Color

type Outputter =
  { log :: String -> Aff Unit
  , debug :: String -> Aff Unit
  , err :: String -> Aff Unit
  , write  :: String -> Aff Unit
  , bolded :: String -> Aff Unit
  , monochrome :: Boolean
  }

-- | Get an outputter, with monochrome based on the command line arguments.
getOutputter :: Args -> Aff Outputter
getOutputter args = do
  -- Bit of a hack, this is used in `pulp server`
  q <- getFlag "_silenced" args.commandOpts
  if q
    then pure nullOutputter
    else makeOutputter <$> getFlag "monochrome" args.globalOpts <*> getFlag "debug" args.globalOpts

-- | Get an outputter. The argument represents "monochrome"; if true is
-- | supplied, the returned logger will never use color. Otherwise, whether or
-- | not colour is used depends on the "supports-color" module. Note that the
-- | `monochrome` attribute of the returned outputter might not necessarily
-- | be the same as the argument supplied.
makeOutputter :: Boolean -> Boolean -> Outputter
makeOutputter monochrome enableDebug =
  if not monochrome && Color.hasBasic
    then ansiOutputter enableDebug
    else monochromeOutputter enableDebug

monochromeOutputter :: Boolean -> Outputter
monochromeOutputter enableDebug =
  { log: monobullet
  , debug: if enableDebug then monobullet else dud
  , err: monobullet
  , write: write stderr
  , bolded: write stderr
  , monochrome: true
  }
  where
  monobullet text = write stderr ("* " <> text <> "\n")

ansiOutputter :: Boolean -> Outputter
ansiOutputter enableDebug =
  { log: bullet stderr Green
  , debug: if enableDebug then bullet stderr Yellow else dud
  , err: bullet stderr Red
  , write: write stderr
  , bolded: write stderr <<< withGraphics bold
  , monochrome: false
  }

bullet :: WritableStream -> Color -> String -> Aff Unit
bullet stream color text = do
  write stream (withGraphics (foreground color) "* ")
  write stream (text <> "\n")

-- | An outputter which doesn't ever output anything.
nullOutputter :: Outputter
nullOutputter =
  { log: dud
  , debug: dud
  , err: dud
  , write: dud
  , bolded: dud
  , monochrome: false
  }

dud :: String -> Aff Unit
dud = const (pure unit)
