
module Pulp.Docs where

import Prelude
import Control.Bind ((>=>), join)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.String.Regex (Regex(), regex, noFlags, match, replace)
import Data.Foldable (mconcat, for_)
import Control.Monad
import Control.Monad.Eff.Class (liftEff)

import Pulp.Args
import Pulp.Args.Get
import Pulp.Exec
import Pulp.Files
import qualified Pulp.System.Process as Process
import qualified Pulp.System.Log as Log

action :: Action
action = Action \args -> do
  cwd <- liftEff Process.cwd
  Log.log $ "Generating documentation in " ++ cwd

  let opts = Map.union args.globalOpts args.commandOpts

  withTests <- getFlag "withTests" opts
  globTest <- if withTests then testGlobs opts else pure Set.empty
  globSrc <- Set.union globTest <$> defaultGlobs opts
  globGen <- Set.union globTest <$> localGlobs opts

  genFiles <- resolveGlobs (sources globGen)

  Tuple docgen fails <- pure $ mconcat $ flip map genFiles \path -> do
    case matchModuleName path of
      Just mn ->
        Tuple ["--docgen", mn ++ ":" ++ docPathFor path] []
      Nothing ->
        Tuple [] [path]

  unless (Array.null fails) $ do
    Log.log "Unable to generate documentation for the following modules:"
    for_ fails (Log.log <<< ("  " ++))
    Log.log "Please make sure your module names are consistent with your file paths."

  _ <- execQuiet "psc-docs" (args.remainder ++ sources globSrc ++ docgen) Nothing

  Log.log "Documentation generated."

moduleNameR :: Regex
moduleNameR = regex "([A-Z][^\\/\\.]*(\\/|\\.))+" noFlags

r :: String -> Regex
r str = regex str noFlags

matchModuleName :: String -> Maybe String
matchModuleName =
  match moduleNameR
    >=> Array.head
    >>> join
    >>> map (replace (r "\\.$") "" >>> replace pathSeparators ".")
  where
  pathSeparators :: Regex
  pathSeparators = regex "(\\/|\\\\)" (noFlags { global = true })

docPathFor :: String -> String
docPathFor = replace (r ".purs$") ".md" <<< replace (r "^(src|test)") "docs"
