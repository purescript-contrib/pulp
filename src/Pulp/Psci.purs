
module Pulp.Psci where

import Prelude
import Data.Maybe
import Data.Map as Map
import Data.Array as Array
import Data.String as String
import Data.Set as Set
import Data.Foldable (all)
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Error.Class
import Node.FS.Aff as FS
import Node.Encoding (Encoding(UTF8))

import Pulp.System.FFI
import Pulp.System.Files as Files
import Pulp.Args
import Pulp.Exec
import Pulp.Files

action :: Action
action = Action \args -> do
  updateConfig args
  execInteractive "psci" args.remainder Nothing

updateConfig :: Args -> AffN Unit
updateConfig args = do
  mpsci <- suppressENOENT (FS.readTextFile UTF8 ".psci")
  let entries = case mpsci of
                  Just psci -> extractPsciEntries psci
                  Nothing -> []

  let opts = Map.union args.commandOpts args.globalOpts
  globs <- Set.union <$> defaultGlobs opts <*> testGlobs opts

  deps <- resolveGlobs (sources globs)
  ffis <- resolveGlobs (ffis globs)

  let psci = Array.concat [ entries
                          , map (":load " <> _) deps
                          , map (":foreign " <> _) ffis
                          ]

  FS.writeTextFile UTF8 ".psci" (String.joinWith "\n" psci ++ "\n")


-- | Extract all entries from a .psci file, apart from :load or :foreign.
extractPsciEntries :: String -> Array String
extractPsciEntries =
  String.split "\n"
  >>> Array.filter (allOf [ maybe false (_ /= 0) <<< String.indexOf ":load "
                          , maybe false (_ /= 0) <<< String.indexOf ":foreign "
                          , (_ > 0) <<< String.length <<< String.trim
                          ])
  where
  allOf fs x = all (_ $ x) fs

suppressENOENT :: forall m a. (MonadError Error m) => m a -> m (Maybe a)
suppressENOENT act = do
  catchJust (\err -> if Files.isENOENT err then Just unit else Nothing)
            (Just <$> act)
            (const (pure Nothing))
