
module Pulp.Files where

import Prelude
import Data.Foreign.Class (IsForeign)
import Data.List (fromList)
import Data.Set (Set(), union, toList, singleton)
import qualified Node.Path as Path

import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get

sources :: Set String -> Array String
sources = toList >>> fromList >>> map (++ "/**/*.purs")

ffis :: Set String -> Array String
ffis = toList >>> fromList >>> map (++ "/**/*.js")

fromOption :: forall e a. (IsForeign a) => (a -> a) -> String -> Options -> AffN e (Set a)
fromOption f name opts = do
  value <- getOption' name opts
  pure $ singleton (f value)

localGlobs :: forall e. Options -> AffN e (Set String)
localGlobs = fromOption id "srcPath"

testGlobs :: forall e. Options -> AffN e (Set String)
testGlobs = fromOption id "testPath"

dependencyGlobs :: forall e. Options -> AffN e (Set String)
dependencyGlobs =
  fromOption (\path -> Path.concat [path, "purescript-*", "src"])
             "dependencyPath"

defaultGlobs :: forall e. Options -> AffN e (Set String)
defaultGlobs args =
  union <$> localGlobs args <*> dependencyGlobs args

outputModules :: String -> Set String
outputModules buildPath =
  singleton (buildPath ++ "/*/*.js")
