
module Pulp.Init
  ( action
  ) where

import Prelude
import Data.String (joinWith)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Class (liftEff)
import Node.Path as Path
import Node.FS.Aff (writeTextFile, exists)
import Node.Encoding (Encoding(UTF8))

import Pulp.Outputter
import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get (getFlag)
import Pulp.System.Files (mkdirIfNotExist)
import Pulp.System.Process as Process
import Pulp.Bower (launchBower)

foreign import bowerFile :: String -> String

unlines :: Array String -> String
unlines arr = joinWith "\n" arr ++ "\n"

gitignore :: String
gitignore = unlines [
  "/bower_components/",
  "/node_modules/",
  "/.pulp-cache/",
  "/output/",
  "/.psci*",
  "/src/.webpack.js"
  ]

mainFile :: String
mainFile = unlines [
  "module Main where",
  "",
  "import Prelude",
  "import Control.Monad.Eff",
  "import Control.Monad.Eff.Console",
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
  "import Control.Monad.Eff",
  "import Control.Monad.Eff.Console",
  "",
  "main :: forall e. Eff (console :: CONSOLE | e) Unit",
  "main = do",
  "  log \"You should add some tests.\""
  ]

init :: forall e. Outputter e -> AffN Unit
init out = do
  cwd <- liftEff Process.cwd
  let name = Path.basename cwd
  out.log $ "Generating project skeleton in " ++ cwd

  writeFile (Path.concat [cwd, ".gitignore"]) gitignore
  writeFile (Path.concat [cwd, "bower.json"]) (bowerFile name)

  mkdirIfNotExist "src"
  writeFile (Path.concat [cwd, "src", "Main.purs"]) mainFile

  mkdirIfNotExist "test"
  writeFile (Path.concat [cwd, "test", "Main.purs"]) testFile

  launchBower ["update"]

  where
  writeFile = writeTextFile UTF8

action :: Action
action = Action \args -> do
  force     <- getFlag "force" args.commandOpts
  cwd       <- liftEff Process.cwd
  doesExist <- exists $ Path.concat [cwd, "bower.json"]

  if (doesExist && not force)
    then
      throwError $ error $
        "There's already a project here. Run `pulp init --force` if you're "
        ++ "sure you want to overwrite it."
    else do
      out <- getOutputter args
      init out
