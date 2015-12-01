module Pulp.Mtime
  ( maxMtime
  , maxMtime'
  ) where

import Prelude
import Data.Maybe
import Data.Date (Date())
import Data.Monoid
import Data.Foldable (mconcat)
import Data.Traversable (traverse) 
import Data.Ord (Max(..), runMax)
import Control.Alt
import Node.FS.Stats as FS
import Node.FS.Aff as FS
import Node.Path as Path

import Pulp.System.FFI

-- | Scan a path recursively and return the time of the most recent
-- | modification of any file within. If an error occurs, this function just
-- | returns Nothing.
maxMtime :: forall e. String -> AffN e (Maybe Date)
maxMtime path =
  maxMtime' path <|> pure Nothing

-- | A version of maxMtime which does not ignore errors.
maxMtime' :: forall e. String -> AffN e (Maybe Date)
maxMtime' =
  map (map runMax)
    <<< scanDirectoryRecursive (Just <<< Max <<< FS.modifiedTime)

-- | Scan a directory recursively, calling the provided function on every file
-- | encountered, and collecting the results in some monoid.
scanDirectoryRecursive :: forall e a. (Monoid a) =>
  (FS.Stats -> a) -> String -> AffN e a
scanDirectoryRecursive f path = do
  stat <- FS.stat path
  if FS.isFile stat
    then pure (f stat)
    else if FS.isDirectory stat
      then do
        files <- FS.readdir path
        mconcat <$> traverse (scanDirectoryRecursive f)
                             (map (Path.resolve [path]) files)
      else
        pure mempty
