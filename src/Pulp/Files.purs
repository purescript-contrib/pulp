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

import Data.Array (concat, mapMaybe)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (stripSuffix, Pattern(..), split)
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff)
import Foreign.Class (class Decode)
import Node.Path as Path
import Pulp.Args (Options)
import Pulp.Args.Get (getOption, getOption')
import Pulp.Exec (execQuiet)
import Pulp.Project (usingPscPackage)
import Pulp.System.FFI (Callback, runNode)

recursiveGlobWithExtension :: String -> Set String -> Array String
recursiveGlobWithExtension ext =
  Set.toUnfoldable >>> map (_ <> ("/**/*." <> ext))

sources :: Set String -> Array String
sources = recursiveGlobWithExtension "purs"

ffis :: Set String -> Array String
ffis = recursiveGlobWithExtension "js"

globsFromOption' :: forall a. Decode a => (a -> a) -> String -> Options -> Aff (Set a)
globsFromOption' f name opts = do
  value <- getOption name opts
  pure $ case value of
          Just v  -> Set.singleton (f v)
          Nothing -> Set.empty

globsFromOption :: forall a. Decode a => String -> Options -> Aff (Set a)
globsFromOption = globsFromOption' identity

localGlobs :: Options -> Aff (Set String)
localGlobs = globsFromOption "srcPath"

testGlobs :: Options -> Aff (Set String)
testGlobs = globsFromOption "testPath"

dependencyGlobs :: Options -> Aff (Set String)
dependencyGlobs opts = do
  p <- getOption' "_project" opts
  if usingPscPackage p
    then pscPackageGlobs
    else globsFromOption' (\path -> Path.concat [path, "purescript-*", "src"])
         "dependencyPath" opts

pscPackageGlobs :: Aff (Set String)
pscPackageGlobs =
  execQuiet "psc-package" ["sources"] Nothing <#> processGlobs
  where
    -- Split on newlines and strip the /**/*/.purs suffixes just to
    -- append them later so it plays well with the other globs
    processGlobs =
      (split (Pattern "\r\n") >=> split (Pattern "\n")) >>>
      mapMaybe (stripSuffix (Pattern (Path.sep <> "**" <> Path.sep <> "*.purs"))) >>>
      Set.fromFoldable

includeGlobs :: Options -> Aff (Set String)
includeGlobs opts = mkSet <$> getOption "includePaths" opts
  where
  mkSet = Set.fromFoldable <<< fromMaybe []

defaultGlobs :: Options -> Aff (Set String)
defaultGlobs opts =
  Set.unions <$> sequence (List.fromFoldable
                            [ localGlobs opts
                            , dependencyGlobs opts
                            , includeGlobs opts
                            ])

outputModules :: String -> Array String
outputModules buildPath =
  [buildPath <> "/*/*.js"]

resolveGlobs :: Array String -> Aff (Array String)
resolveGlobs patterns = concat <$> traverse glob patterns

foreign import globImpl :: Fn2 String (Callback (Array String)) Unit

glob :: String -> Aff (Array String)
glob pattern = runNode $ runFn2 globImpl pattern
