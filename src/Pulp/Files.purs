
module Pulp.Files
  ( sources
  , ffis
  , localGlobs
  , testGlobs
  , dependencyGlobs
  , includeGlobs
  , defaultGlobs
  , outputModules
  , resolveGlobs
  , glob
  ) where

import Prelude
import Data.Array (concat)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function
import Data.Foreign.Class (class IsForeign)
import Data.List as List
import Data.Set (Set())
import Data.Set as Set
import Data.Traversable (sequence, traverse)
import Node.Path as Path

import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get

sources :: Set String -> Array String
sources = Set.toList >>> List.fromList >>> map (_ <> "/**/*.purs")

ffis :: Set String -> Array String
ffis = Set.toList >>> List.fromList >>> map (_ <> "/**/*.js")

globsFromOption' :: forall a. (IsForeign a) => (a -> a) -> String -> Options -> AffN (Set a)
globsFromOption' f name opts = do
  value <- getOption name opts
  pure $ case value of
          Just v  -> Set.singleton (f v)
          Nothing -> Set.empty

globsFromOption :: forall a. (IsForeign a) => String -> Options -> AffN (Set a)
globsFromOption = globsFromOption' id

localGlobs :: Options -> AffN (Set String)
localGlobs = globsFromOption "srcPath"

testGlobs :: Options -> AffN (Set String)
testGlobs = globsFromOption "testPath"

dependencyGlobs :: Options -> AffN (Set String)
dependencyGlobs =
  globsFromOption' (\path -> Path.concat [path, "purescript-*", "src"])
                   "dependencyPath"

includeGlobs :: Options -> AffN (Set String)
includeGlobs opts = mkSet <$> getOption "includePaths" opts
  where
  mkSet = Set.fromList <<< List.toList <<< fromMaybe []

defaultGlobs :: Options -> AffN (Set String)
defaultGlobs opts =
  Set.unions <$> sequence (List.toList
                            [ localGlobs opts
                            , dependencyGlobs opts
                            , includeGlobs opts
                            ])

outputModules :: String -> Array String
outputModules buildPath =
  [buildPath ++ "/*/*.js"]

resolveGlobs :: Array String -> AffN (Array String)
resolveGlobs patterns = concat <$> traverse glob patterns

foreign import glob' :: Fn2 String (Callback (Array String)) Unit

glob :: String -> AffN (Array String)
glob pattern = runNode $ runFn2 glob' pattern
