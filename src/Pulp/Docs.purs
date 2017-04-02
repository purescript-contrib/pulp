
module Pulp.Docs where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Set as Set
import Data.Map as Map
import Data.String as String
import Data.Foldable (fold, for_, elem)
import Data.Traversable (traverse)
import Control.Monad.Eff.Class (liftEff)
import Node.FS.Aff as FS
import Node.Encoding (Encoding(..))
import Node.Process as Process

import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec
import Pulp.Files
import Pulp.Outputter
import Pulp.System.FFI

action :: Action
action = Action \args -> do
  out <- getOutputter args

  cwd <- liftEff Process.cwd
  out.log $ "Generating documentation in " <> cwd

  let opts = Map.union args.globalOpts args.commandOpts

  withTests <- getFlag "withTests" opts
  withDeps <- getFlag "withDependencies" opts

  let includeWhen b act = if b then act else pure Set.empty
  optionalExtras <- Set.union <$> includeWhen withTests (testGlobs opts)
                              <*> includeWhen withDeps  (dependencyGlobs opts)

  globSrc <- Set.union optionalExtras <$> defaultGlobs opts
  globGen <- Set.union optionalExtras <$> localGlobs opts

  genFiles <- resolveGlobs (sources globGen)

  Tuple docgen fails <- fold <$> traverse makeDocgen genFiles

  unless (Array.null fails) $ do
    out.err $ "Unable to extract module name from the following modules:"
    for_ fails (out.log <<< ("  " <> _))
    out.err $ "This may be a bug."

  _ <- execQuiet "purs" (["docs"] <> args.remainder <> sources globSrc <> docgen) Nothing

  out.log "Documentation generated."

-- | Given a file path to be included in the documentation, return a --docgen
-- | argument for it, to be passed to `purs docs`.
makeDocgen :: String -> AffN (Tuple (Array String) (Array String))
makeDocgen path = do
  maybeModName <- extractModuleName path
  pure $ case maybeModName of
    Just mn ->
      Tuple ["--docgen", showModuleName mn <> ":" <> docPath mn] []
    Nothing ->
      Tuple [] [path]

-- | Given a module name, return the file path where its documentation should
-- | be written to.
docPath :: ModuleName -> String
docPath mn = "generated-docs/" <> String.joinWith "/" mn <> ".md"

type ModuleName = Array String

showModuleName :: ModuleName -> String
showModuleName = String.joinWith "."

-- | Given a PureScript source file path, extract its module name (or throw
-- | an error).
extractModuleName :: String -> AffN (Maybe ModuleName)
extractModuleName path = go <$> FS.readTextFile UTF8 path
  where
  go = String.split (String.Pattern "\n")
        >>> map moduleNameFromLine
        >>> Array.catMaybes
        >>> Array.head

-- | Given a line in a PureScript source file, attempt to extract its name.
moduleNameFromLine :: String -> Maybe ModuleName
moduleNameFromLine =
  String.stripPrefix (String.Pattern "module ")
  >>> map (   String.takeWhile (not <<< (_ `elem` [' ', '(']))
          >>> String.split (String.Pattern ".")
          )
