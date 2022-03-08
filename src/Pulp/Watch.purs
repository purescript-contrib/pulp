module Pulp.Watch
  ( watch
  , watchAff
  , watchDirectories
  , action
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Foldable (notElem)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Now as Now
import Effect.Ref as Ref
import Node.ChildProcess (fork, pid)
import Node.Globals (__filename)
import Node.Process as Process
import Pulp.Args (Action(..), Options)
import Pulp.Args.Get (getOption)
import Pulp.Files (defaultGlobs, ffis, sources, testGlobs)
import Pulp.Outputter (getOutputter)
import Pulp.System.TreeKill (treeKill)
import Pulp.Utils (orErr)

foreign import watch ::
  Array String
  -> (String -> Effect Unit)
  -> Effect Unit

watchAff ::  Array String -> (String -> Aff Unit) -> Aff Unit
watchAff dirs callback = liftEffect do
  debouncedCallback <- debounce (Milliseconds 100.0)
                                (callback
                                >>> launchAff
                                >>> void)
  watch dirs debouncedCallback

-- | Ensure that a callback is only called at some given maximum frequency,
-- | by returning a new callback that does nothing if an attempt is made to
-- | perform it again sooner than the given duration since the last attempt.
debounce :: forall a. Milliseconds -> (a -> Effect Unit) -> Effect (a -> Effect Unit)
debounce cooldown callback = do
  timer <- Ref.new (bottom :: DateTime)
  pure \info -> do
    lastPerformed <- Ref.read timer
    now <- Now.nowDateTime
    when (DateTime.diff now lastPerformed > cooldown) do
      Ref.write now timer
      callback info

-- Returns Nothing if the given Options did not include the relevant options
-- i.e. watching does not make sense with this command.
watchDirectories :: Options -> Aff (Maybe (Array String))
watchDirectories opts = do
  -- If any of these give Nothing, we shouldn't be using watchDirectories
  let basicPathOpts = ["srcPath", "testPath", "dependencyPath"]
  basicPaths <- traverse (flip getOption opts) basicPathOpts

  -- It's ok if this is Nothing, though.
  includePaths <- fromMaybe [] <$> getOption "includePaths" opts

  pure $ map (_ <> includePaths) (sequence basicPaths)

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  -- It is important to do this before attempting to `fork` a separate process.
  _ <- watchDirectories opts >>= orErr "This command does not work with --watch"

  argv' <- liftEffect $ Array.filter (_ `notElem` ["-w", "--watch"]) <<< Array.drop 2 <$> Process.argv
  childV <- AVar.empty
  liftEffect (fork __filename argv') >>= \x -> AVar.put x childV

  globs <- Set.union <$> defaultGlobs opts <*> testGlobs opts
  let fileGlobs = sources globs <> ffis globs

  watchAff fileGlobs $ \_ -> do
    child <- AVar.take childV
    liftEffect $ treeKill (pid child) "SIGTERM"
    out.write "---\n"
    out.log "Source tree changed; restarting:"
    liftEffect (fork __filename argv') >>= AVar.put <@> childV
