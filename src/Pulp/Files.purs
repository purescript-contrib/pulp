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
import Control.Monad.Except (runExcept)
import Data.Array (concat, mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Function.Uncurried
import Data.Foreign (readString)
import Data.Foreign.Index (readProp)
import Data.Foreign.Class (class Decode)
import Data.List as List
import Data.String (stripSuffix, Pattern(..))
import Data.String.Regex (split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Set (Set())
import Data.Set as Set
import Data.Traversable (sequence, traverse)
import Node.Path as Path

import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec (execQuiet)
import Pulp.Project (Project(..))

recursiveGlobWithExtension :: String -> Set String -> Array String
recursiveGlobWithExtension ext =
  Set.toUnfoldable >>> map (_ <> ("/**/*." <> ext))

sources :: Set String -> Array String
sources = recursiveGlobWithExtension "purs"

ffis :: Set String -> Array String
ffis = recursiveGlobWithExtension "js"

globsFromOption' :: forall a. Decode a => (a -> a) -> String -> Options -> AffN (Set a)
globsFromOption' f name opts = do
  value <- getOption name opts
  pure $ case value of
          Just v  -> Set.singleton (f v)
          Nothing -> Set.empty

globsFromOption :: forall a. Decode a => String -> Options -> AffN (Set a)
globsFromOption = globsFromOption' id

localGlobs :: Options -> AffN (Set String)
localGlobs = globsFromOption "srcPath"

testGlobs :: Options -> AffN (Set String)
testGlobs = globsFromOption "testPath"

dependencyGlobs :: Options -> AffN (Set String)
dependencyGlobs opts = do
  Project pro <- getOption' "_project" opts
  -- If project file has a `set` property we assume it's a psc-package project file
  case runExcept (readProp "set" pro.projectFile >>= readString) of
    Right _ -> pscPackageGlobs
    _ -> globsFromOption' (\path -> Path.concat [path, "purescript-*", "src"]) "dependencyPath" opts

pscPackageGlobs :: AffN (Set String)
pscPackageGlobs = do
  execQuiet "psc-package" ["sources"] Nothing <#>
  Set.fromFoldable <<<
  -- Strip the glob.purs suffixes just to append them later so it plays well with the other globs
  mapMaybe (stripSuffix (Pattern "/**/*.purs")) <<<
  split (unsafeRegex "\r\n|\n" noFlags)

includeGlobs :: Options -> AffN (Set String)
includeGlobs opts = mkSet <$> getOption "includePaths" opts
  where
  mkSet = Set.fromFoldable <<< fromMaybe []

defaultGlobs :: Options -> AffN (Set String)
defaultGlobs opts =
  Set.unions <$> sequence (List.fromFoldable
                            [ localGlobs opts
                            , dependencyGlobs opts
                            , includeGlobs opts
                            ])

outputModules :: String -> Array String
outputModules buildPath =
  [buildPath <> "/*/*.js"]

resolveGlobs :: Array String -> AffN (Array String)
resolveGlobs patterns = concat <$> traverse glob patterns

foreign import glob' :: Fn2 String (Callback (Array String)) Unit

glob :: String -> AffN (Array String)
glob pattern = runNode $ runFn2 glob' pattern
