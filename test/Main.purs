module Test.Main where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Data.Version (Version, version)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Process as Process
import Pulp.Exec (execQuiet)
import Pulp.Git (getVersionFromGitTag)
import Pulp.System.Files (tempDir)
import Test.Assert (assertEqual)

-- | Run a shell command
run :: String -> Array String -> Aff String
run cmd args = execQuiet cmd args Nothing

run_ :: String -> Array String -> Aff Unit
run_ cmd args = void $ run cmd args

commitFile :: String -> String -> Aff Unit
commitFile name contents = do
  FS.writeTextFile UTF8 name contents
  run_ "git" ["add", name]
  run_ "git" ["commit", "--message", "add " <> name]

assertEq :: forall a. Show a => Eq a => a -> a -> Aff Unit
assertEq expected actual =
  liftEffect $ assertEqual { actual, expected }

main = launchAff do
  dir <- tempDir { prefix: "pulp-unit-test-", suffix: "" }
  liftEffect $ Process.chdir dir
  run_ "git" ["init"]
  run_ "git" ["config", "user.email", "pulp-test@example.com"]
  run_ "git" ["config", "user.name", "Pulp Tester"]

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
