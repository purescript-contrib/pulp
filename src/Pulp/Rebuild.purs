module Pulp.Rebuild
  ( needs
  , touch
  ) where

import Prelude
import Control.Alt
import Data.Maybe
import Data.Map as Map
import Data.Ord
import Data.Date
import Data.Foldable
import Data.Traversable
import Node.Path as Path
import Node.FS.Aff as FS
import Node.Encoding (Encoding(UTF8))

import Pulp.Args
import Pulp.Args.Get
import Pulp.System.FFI
import Pulp.Project
import Pulp.Mtime

needs :: Args -> Array String -> AffN Boolean
needs args paths = do
  let opts = Map.union args.globalOpts args.commandOpts

  force <- getFlag "force" opts
  if force
    then pure true
    else do
      proj <- getOption' "_project" opts

      liveStamp <- map runMax <<< mconcat <$> traverse mtime paths
      cacheStamp <- maxMtime (buildStampPath proj)

      hasSameArgs <- sameArgs proj args

      pure (liveStamp > cacheStamp || not hasSameArgs)

  where
  mtime :: String -> AffN (Maybe (Max Date))
  mtime = map (map Max) <<< maxMtime

touch :: Args -> AffN Unit
touch args = do
  let opts = Map.union args.globalOpts args.commandOpts
  proj <- getOption' "_project" opts
  FS.writeTextFile UTF8 (buildStampPath proj) (hashAny args)

sameArgs :: Project -> Args -> AffN Boolean
sameArgs proj args = go <|> pure false
  where
  go = do
    stamp <- FS.readTextFile UTF8 (buildStampPath proj)
    pure (stamp == hashAny args)

buildStampPath :: Project -> String
buildStampPath (Project pro) = Path.resolve [pro.cache] "build-stamp"

foreign import hashAny :: forall a. a -> String
