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
import Data.List (List(Nil), fromFoldable)
import Data.Foldable (for_)
import Data.Version.Haskell as HVer

import Pulp.Outputter
import Pulp.System.FFI
import Pulp.Args
import Pulp.Args.Get (getFlag)
import Pulp.System.Files (mkdirIfNotExist)
import Pulp.PackageManager (launchBower, launchPscPackage)
import Pulp.Validate (getPursVersion)
import Pulp.Utils (throw)

foreign import bowerFile :: String -> String

data InitStyle = Bower | PscPackage

data EffOrEffect = UseEff | UseEffect

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

mainFile :: EffOrEffect -> String
mainFile = case _ of
  UseEffect -> unlines [
    "module Main where",
    "",
    "import Prelude",
    "import Effect (Effect)",
    "import Effect.Console (log)",
    "",
    "main :: Effect Unit",
    "main = do",
    "  log \"Hello sailor!\""
    ]
  UseEff -> unlines [
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

testFile :: EffOrEffect -> String
testFile = case _ of
  UseEffect -> unlines [
    "module Test.Main where",
    "",
    "import Prelude",
    "import Effect (Effect)",
    "import Effect.Console (log)",
    "",
    "main :: Effect Unit",
    "main = do",
    "  log \"You should add some tests.\""
    ]
  UseEff -> unlines [
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

projectFiles :: InitStyle -> EffOrEffect -> String -> String -> Array { path :: String, content :: String }
projectFiles initStyle effOrEffect pathRoot projectName =
  case initStyle of
    Bower      -> cons bowerJson common
    PscPackage -> common
  where
  fullPath pathParts = Path.concat ([pathRoot] <> pathParts)
  bowerJson = { path: fullPath ["bower.json"],        content: bowerFile projectName }
  common  = [ { path: fullPath [".gitignore"],        content: gitignore }
            , { path: fullPath [".purs-repl"],        content: pursReplFile }
            , { path: fullPath ["src", "Main.purs"],  content: mainFile effOrEffect }
            , { path: fullPath ["test", "Main.purs"], content: testFile effOrEffect }
            ]

init :: InitStyle -> EffOrEffect -> Boolean -> Outputter -> AffN Unit
init initStyle effOrEffect force out = do
  cwd <- liftEff Process.cwd
  let projectName = Path.basename cwd
  out.log $ "Generating project skeleton in " <> cwd

  let files = projectFiles initStyle effOrEffect cwd projectName

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

  install initStyle effOrEffect

  where
    install Bower UseEff = do
      launchBower ["install", "--save", "purescript-prelude@3.3.0", "purescript-console@3.0.0"]
      launchBower ["install", "--save-dev", "purescript-psci-support@3.0.0"]

    install Bower UseEffect = do
      launchBower ["install", "--save", "purescript-prelude", "purescript-console", "purescript-effect"]
      launchBower ["install", "--save-dev", "purescript-psci-support"]

    install PscPackage UseEff = do
      launchPscPackage ["init"]
      launchPscPackage ["install", "eff"]
      launchPscPackage ["install", "console"]
      launchPscPackage ["install", "psci-support"]

    install PscPackage UseEffect = do
      launchPscPackage ["init"]
      launchPscPackage ["install", "effect"]
      launchPscPackage ["install", "console"]
      launchPscPackage ["install", "psci-support"]

action :: Action
action = Action \args -> do
  force       <- getFlag "force" args.commandOpts
  pscPackage  <- getFlag "pscPackage" args.globalOpts
  withEff     <- getFlag "withEff" args.commandOpts
  withEffect  <- getFlag "withEffect" args.commandOpts
  out         <- getOutputter args
  effOrEffect <- getEffOrEffect out withEff withEffect

  if withEff && withEffect
    then throw "Cannot specify both --with-eff and --with-effect. Please choose one and try again."
    else init (if pscPackage then PscPackage else Bower) effOrEffect force out

  where

  minEffectVersion = HVer.Version (fromFoldable [0, 12, 0]) Nil

  getEffOrEffect out withEff withEffect
    | withEff    = pure UseEff
    | withEffect = pure UseEffect
    | otherwise  = do
        ver <- getPursVersion out
        if ver < minEffectVersion
          then pure UseEff
          else pure UseEffect
