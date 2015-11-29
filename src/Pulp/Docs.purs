
module Pulp.Docs where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.String as String
import Data.Foldable (mconcat, for_, elem)
import Data.Traversable (traverse)
import Control.Monad
import Control.Monad.Eff.Class (liftEff)
import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))

import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec
import Pulp.Files
import qualified Pulp.System.Process as Process
import qualified Pulp.System.Log as Log
import Pulp.System.FFI

action :: Action
action = Action \args -> do
  cwd <- liftEff Process.cwd
  Log.log $ "Generating documentation in " ++ cwd

  let opts = Map.union args.globalOpts args.commandOpts

  withTests <- getFlag "withTests" opts
  withDeps <- getFlag "withDependencies" opts

  let includeWhen b act = if b then act else pure Set.empty
  optionalExtras <- Set.union <$> includeWhen withTests (testGlobs opts)
                              <*> includeWhen withDeps  (dependencyGlobs opts)

  globSrc <- Set.union optionalExtras <$> defaultGlobs opts
  globGen <- Set.union optionalExtras <$> localGlobs opts

  genFiles <- resolveGlobs (sources globGen)

  Tuple docgen fails <- mconcat <$> traverse makeDocgen genFiles

  unless (Array.null fails) $ do
    Log.err $ "Unable to extract module name from the following modules:"
    for_ fails (Log.log <<< ("  " ++))
    Log.err $ "This may be a bug."

  _ <- execQuiet "psc-docs" (args.remainder ++ sources globSrc ++ docgen) Nothing

  Log.log "Documentation generated."

-- | Given a file path to be included in the documentation, return a --docgen
-- | argument for it, to be passsed to psc-docs.
makeDocgen :: forall e. String -> AffN e (Tuple (Array String) (Array String))
makeDocgen path = do
  maybeModName <- extractModuleName path
  pure $ case maybeModName of
    Just mn ->
      Tuple ["--docgen", showModuleName mn ++ ":" ++ docPath mn] []
    Nothing ->
      Tuple [] [path]

-- | Given a module name, return the file path where its documentation should
-- | be written to.
docPath :: ModuleName -> String
docPath mn = "docs/" ++ String.joinWith "/" mn ++ ".md"

type ModuleName = Array String

showModuleName :: ModuleName -> String
showModuleName = String.joinWith "."

-- | Given a PureScript source file path, extract its module name (or throw
-- | an error).
extractModuleName :: forall e. String -> AffN e (Maybe ModuleName)
extractModuleName path = go <$> FS.readTextFile UTF8 path
  where
  go = String.split "\n"
        >>> map moduleNameFromLine
        >>> Array.catMaybes
        >>> Array.head

-- | Given a line in a PureScript source file, attempt to extract its name.
moduleNameFromLine :: String -> Maybe ModuleName
moduleNameFromLine =
  String.stripPrefix "module "
  >>> map (   String.takeWhile (not <<< (`elem` [' ', '(']))
          >>> String.split "."
          )
