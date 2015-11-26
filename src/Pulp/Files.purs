
module Pulp.Files where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Foreign.Class (IsForeign)
import Data.List (fromList)
import Data.Set (Set(), union, toList, singleton, empty)
import qualified Node.Path as Path

import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get

sources :: Set String -> Array String
sources = toList >>> fromList >>> map (++ "/**/*.purs")

ffis :: Set String -> Array String
ffis = toList >>> fromList >>> map (++ "/**/*.js")

globsFromOption' :: forall e a. (IsForeign a) => (a -> a) -> String -> Options -> AffN e (Set a)
globsFromOption' f name opts = do
  value <- getOption name opts
  pure $ case value of
          Just v  -> singleton (f v)
          Nothing -> empty

globsFromOption :: forall e a. (IsForeign a) => String -> Options -> AffN e (Set a)
globsFromOption = globsFromOption' id

localGlobs :: forall e. Options -> AffN e (Set String)
localGlobs = globsFromOption "srcPath"

testGlobs :: forall e. Options -> AffN e (Set String)
testGlobs = globsFromOption "testPath"

dependencyGlobs :: forall e. Options -> AffN e (Set String)
dependencyGlobs =
  globsFromOption' (\path -> Path.concat [path, "purescript-*", "src"])
                   "dependencyPath"

defaultGlobs :: forall e. Options -> AffN e (Set String)
defaultGlobs args =
  union <$> localGlobs args <*> dependencyGlobs args

outputModules :: String -> Set String
outputModules buildPath =
  singleton (buildPath ++ "/*/*.js")
