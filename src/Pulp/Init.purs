module Pulp.Init
  ( action
  ) where

import Prelude
import Data.Array (cons)
import Data.String (joinWith)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff)
import Node.Path as Path
import Node.FS.Aff (writeTextFile, exists)
import Node.Encoding (Encoding(UTF8))
import Node.Process as Process
import Data.Foldable (for_)

import Pulp.Outputter
import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get (getFlag)
import Pulp.System.Files (mkdirIfNotExist)
import Pulp.PackageManager (launchBower, launchPscPackage)

foreign import bowerFile :: String -> String

unlines :: Array String -> String
unlines arr = joinWith "\n" arr <> "\n"

gitignore :: String
gitignore = unlines [
  "/bower_components/",
  "/node_modules/",
  "/.pulp-cache/",
  "/output/",
  "/generated-docs/",
  "/.psc-package/",
  "/.psc*",
  "/.purs*",
  "/.psa*"
  ]

pursReplFile :: String
pursReplFile = unlines [
  "import Prelude"
  ]

mainFile :: String
mainFile = unlines [
  "module Main where",
  "",
  "import Prelude",
  "import Control.Monad.Eff (Eff)",
  "import Control.Monad.Eff.Console (CONSOLE, log)",
  "",
  "main :: forall e. Eff (console :: CONSOLE | e) Unit",
  "main = do",
  "  log \"Hello sailor!\""
  ]

testFile :: String
testFile = unlines [
  "module Test.Main where",
  "",
  "import Prelude",
  "import Control.Monad.Eff (Eff)",
  "import Control.Monad.Eff.Console (CONSOLE, log)",
  "",
  "main :: forall e. Eff (console :: CONSOLE | e) Unit",
  "main = do",
  "  log \"You should add some tests.\""
  ]

projectFiles :: Boolean -> String -> String -> Array { path :: String, content :: String }
projectFiles bower pathRoot projectName =
  let bowerJson = { path: fullPath ["bower.json"],        content: bowerFile projectName }
      def = [ { path: fullPath [".gitignore"],        content: gitignore }
            , { path: fullPath [".purs-repl"],        content: pursReplFile }
            , { path: fullPath ["src", "Main.purs"],  content: mainFile }
            , { path: fullPath ["test", "Main.purs"], content: testFile }
            ]
      in if bower then cons bowerJson def else def
  where
  fullPath pathParts = Path.concat ([pathRoot] <> pathParts)

init :: Boolean -> Boolean -> Outputter -> AffN Unit
init bower force out = do
  cwd <- liftEff Process.cwd
  let projectName = Path.basename cwd
  out.log $ "Generating project skeleton in " <> cwd

  let files = projectFiles bower cwd projectName

  when (not force) do
    for_ files \f -> do
      fileExists <- exists f.path
      when fileExists do
        throwError <<< error $ "Found " <> f.path <> ": "
                               <> "There's already a project here. Run `pulp init --force` "
                               <> "if you're sure you want to overwrite it."

  for_ files \f -> do
    let dir = Path.dirname f.path
    when (dir /= cwd) (mkdirIfNotExist dir)
    writeTextFile UTF8 f.path f.content

  if bower
     then do
          launchBower ["install", "--save", "purescript-prelude", "purescript-console"]
          launchBower ["install", "--save-dev", "purescript-psci-support"]
     else do
          launchPscPackage ["init"]
          launchPscPackage ["install", "eff"]
          launchPscPackage ["install", "console"]

action :: Action
action = Action \args -> do
  force      <- getFlag "force" args.commandOpts
  pscPackage <- getFlag "psc-package" args.commandOpts
  out        <- getOutputter args
  init (not pscPackage) force out
