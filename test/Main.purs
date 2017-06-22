module Test.Main where

import Prelude
import Data.Tuple (Tuple(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Version (Version, version)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff (launchAff)
import Node.Process as Process
import Node.ChildProcess as CP
import Node.FS.Aff as FS
import Node.Encoding (Encoding(UTF8))
import Test.Assert (assert')

import Pulp.Exec (execQuietWithStderr)
import Pulp.System.FFI (AffN, EffN)
import Pulp.System.Files (tempDir)
import Pulp.Git (getVersionFromGitTag)

-- | Run a shell command, swallowing stderr.
run :: String -> Array String -> AffN String
run cmd args = execQuietWithStderr CP.Ignore cmd args Nothing

run_ :: String -> Array String -> AffN Unit
run_ cmd args = void $ run cmd args

commitFile :: String -> String -> AffN Unit
commitFile name contents = do
  FS.writeTextFile UTF8 name contents
  run_ "git" ["add", name]
  run_ "git" ["commit", "--message", "add " <> name]

assertEq :: forall a. Show a => Eq a => a -> a -> AffN Unit
assertEq expected actual =
  liftEff $
    assert' ("Expected " <> show expected <> ", got " <> show actual)
            (expected == actual)

main = launchAff do
  dir <- tempDir { prefix: "pulp-unit-test-", suffix: "" }
  liftEff $ unsafeCoerceEff $ Process.chdir dir
  run_ "git" ["init"]

  -----------------------------------------------------------------------------
  log "getVersionFromGitTag (basic)"

  commitFile "file1.txt" "abcde\n"
  commitFile "file2.txt" "fghij\n"

  run_ "git" ["tag", "v1.0.0"]

  result <- getVersionFromGitTag
  assertEq (Just (Tuple "v1.0.0" (version 1 0 0 Nil Nil))) result

  -----------------------------------------------------------------------------
  log "getVersionFromGitTag doesn't give old tags"

  commitFile "file3.txt" "klmno\n"
  result <- getVersionFromGitTag
  assertEq Nothing result

  -----------------------------------------------------------------------------
  log "getVersionFromGitTag gives the latest version tag pointing to HEAD"

  run_ "git" ["commit", "--allow-empty", "--message", "an empty commit"]
  run_ "git" ["tag", "blahblahblah"]
  run_ "git" ["tag", "v2.0.0"]
  run_ "git" ["tag", "v2.0.1"]

  result <- getVersionFromGitTag
  assertEq (Just (Tuple "v2.0.1" (version 2 0 1 Nil Nil))) result

  -----------------------------------------------------------------------------
  log "getVersionFromGitTag works with annotated tags"

  run_ "git" ["commit", "--allow-empty", "--message", "another empty commit"]
  run_ "git" ["tag", "--annotate", "--message", "lmao", "v3.0.0"]

  result <- getVersionFromGitTag
  assertEq (Just (Tuple "v3.0.0" (version 3 0 0 Nil Nil))) result
