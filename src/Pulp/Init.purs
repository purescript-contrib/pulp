module Pulp.Init
  ( action
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (cons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile, exists)
import Node.Path as Path
import Node.Process as Process
import Pulp.Args (Action(..))
import Pulp.Args.Get (getFlag)
import Pulp.Outputter (Outputter, getOutputter)
import Pulp.PackageManager (launchBower, launchPscPackage)
import Pulp.System.Files (mkdirIfNotExist)
import Pulp.Utils (throw)
import Pulp.Validate (dropPreRelBuildMeta, failIfUsingEsModulesPsVersion, getPursVersion)
import Pulp.Versions.PureScript (psVersions)

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

init :: InitStyle -> EffOrEffect -> Boolean -> Outputter -> Aff Unit
init initStyle effOrEffect force out = do
  cwd <- liftEffect Process.cwd
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

  psVer <- getPursVersion out

  install initStyle effOrEffect (getDepsVersions $ dropPreRelBuildMeta psVer)

  where
    install Bower UseEff p = do
      failIfUsingEsModulesPsVersion out $ Just
        "'purescript-eff' has been archived, so the FFI's CJS modules cannot be migrated to ES modules."
      launchBower ["install", "--save", p.prelude, p.console]
      launchBower ["install", "--save-dev", p.psciSupport]

    install Bower UseEffect p = do
      launchBower ["install", "--save", p.prelude, p.console, p.effect ]
      launchBower ["install", "--save-dev", p.psciSupport ]

    install PscPackage UseEff _ = do
      failIfUsingPscPackageAndEsModules

      launchPscPackage ["init"]
      launchPscPackage ["install", "eff"]
      launchPscPackage ["install", "console"]
      launchPscPackage ["install", "psci-support"]

    install PscPackage UseEffect _ = do
      failIfUsingPscPackageAndEsModules

      launchPscPackage ["init"]
      launchPscPackage ["install", "effect"]
      launchPscPackage ["install", "console"]
      launchPscPackage ["install", "psci-support"]

    failIfUsingPscPackageAndEsModules = do
      failIfUsingEsModulesPsVersion out $ Just
        "'psc-package' not yet supported on a `purs` version that compiles to ES modules."

    getDepsVersions v
      | v >= psVersions.v0_15_0 =
          { prelude: "purescript-prelude@v6.0.0"
          , console: "purescript-console@v6.0.0"
          , effect: "purescript-effect@v4.0.0"
          , psciSupport: "purescript-psci-support@v6.0.0"
          }
      | v >= psVersions.v0_14_0 =
          { prelude: "purescript-prelude@v5.0.1"
          , console: "purescript-console@v5.0.0"
          , effect: "purescript-effect@v3.0.0"
          , psciSupport: "purescript-psci-support@v5.0.0"
          }
      | v >= psVersions.v0_13_0 =
          { prelude: "purescript-prelude@v4.1.1"
          , console: "purescript-console@v4.4.0"
          , effect: "purescript-effect@v2.0.1"
          , psciSupport: "purescript-psci-support@v4.0.0"
          }
      | otherwise =
          { prelude: "purescript-prelude@v4.1.1"
          , console: "purescript-console@v4.4.0"
          , effect: "purescript-effect@v2.0.1"
          , psciSupport: "purescript-psci-support@v4.0.0"
          }

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

  minEffectVersion = psVersions.v0_12_0

  getEffOrEffect out withEff withEffect
    | withEff    = pure UseEff
    | withEffect = pure UseEffect
    | otherwise  = do
        ver <- getPursVersion out
        if (dropPreRelBuildMeta ver) < minEffectVersion
          then pure UseEff
          else pure UseEffect
