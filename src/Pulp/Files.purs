
module Pulp.Files where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foreign.Class (IsForeign)
import qualified Data.List as List
import Data.Set (Set())
import qualified Data.Set as Set
import Data.Traversable (sequence)
import qualified Node.Path as Path

import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get

sources :: Set String -> Array String
sources = Set.toList >>> List.fromList >>> map (++ "/**/*.purs")

ffis :: Set String -> Array String
ffis = Set.toList >>> List.fromList >>> map (++ "/**/*.js")

globsFromOption' :: forall e a. (IsForeign a) => (a -> a) -> String -> Options -> AffN e (Set a)
globsFromOption' f name opts = do
  value <- getOption name opts
  pure $ case value of
          Just v  -> Set.singleton (f v)
          Nothing -> Set.empty

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

includeGlobs :: forall e. Options -> AffN e (Set String)
includeGlobs opts = mkSet <$> getOption "includePaths" opts
  where
  mkSet = Set.fromList <<< List.toList <<< fromMaybe []

defaultGlobs :: forall e. Options -> AffN e (Set String)
defaultGlobs opts =
  Set.unions <$> sequence (List.toList
                            [ localGlobs opts
                            , dependencyGlobs opts
                            , includeGlobs opts
                            ])

outputModules :: String -> Array String
outputModules buildPath =
  [buildPath ++ "/*/*.js"]
