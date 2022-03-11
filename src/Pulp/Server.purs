
module Pulp.Server
  ( action
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff, makeAff)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Path as Path
import Node.Stream as Stream
import Pulp.Args (Action(..), Args, Options)
import Pulp.Args.Get (getFlag, getOption')
import Pulp.Build as Build
import Pulp.Outputter (getOutputter)
import Pulp.System.StaticServer as StaticServer
import Pulp.Utils (orErr)
import Pulp.Validate (failIfUsingEsModulesPsVersion)
import Pulp.Watch (watchAff, watchDirectories)

data BuildResult
  = Succeeded
  | Failed

getBundleFileName :: Options -> Aff String
getBundleFileName opts =
  (_ <> "/app.js") <$> getOption' "buildPath" opts

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  whenM (Build.shouldBundle args) do
    failIfUsingEsModulesPsVersion out $ Just
      "Code path reason: `pulp server` uses `purs bundle` implicitly."

  bundleFileName <- getBundleFileName opts
  hostname <- getOption' "host" opts
  port <- getOption' "port" opts

  -- This AVar should be 'full' (i.e. contain a value) if and only if the
  -- most recent build attempt has finished; in this case, the value inside
  -- tells you whether the build was successful and the bundle can be served
  -- to the client.
  rebuildV <- AVar.empty

  server <- liftEffect $ createServer rebuildV bundleFileName
  listen server { hostname, port, backlog: Nothing }

  out.log $ "Server listening on http://" <> hostname <> ":" <> show port <> "/"

  quiet <- getFlag "quiet" opts
  let rebuild = do
        r <- attempt (rebuildWith { bundleFileName, quiet } args)
        case r of
          Right _ ->
            AVar.put Succeeded rebuildV
          Left _ -> do
            AVar.put Failed rebuildV
            out.err $ "Failed to rebuild; try to fix the compile errors"
  rebuild

  dirs <- watchDirectories opts >>= orErr "Internal error: unexpected Nothing"
  let pattern = map (\d -> Path.concat [d, "**", "*"]) dirs
  watchAff pattern \_ -> do
    void $ AVar.take rebuildV
    rebuild

createServer :: AVar.AVar BuildResult -> String -> Effect HTTP.Server
createServer rebuildV bundleFileName = do
  static <- StaticServer.new "."
  HTTP.createServer \req res ->
    case (HTTP.requestURL req) of
      "/app.js" ->
        void $ launchAff do
          -- The effect of this line should be to block until the current
          -- rebuild is finished (if any).
          r <- AVar.read rebuildV
          liftEffect $ case r of
            Succeeded ->
              StaticServer.serveFile static bundleFileName 200 req res
            Failed -> do
              HTTP.setStatusCode res 400
              HTTP.setStatusMessage res "Rebuild failed"
              let resS = HTTP.responseAsStream res
              void $
                Stream.writeString resS UTF8 "Compile error in pulp server" $
                  Stream.end resS (pure unit)

      _ ->
        StaticServer.serve static req res

listen :: HTTP.Server -> HTTP.ListenOptions -> Aff Unit
listen server opts =
  -- TODO: error handling?
  makeAff \cb -> mempty <* HTTP.listen server opts (cb (Right unit))

rebuildWith :: { bundleFileName :: String, quiet :: Boolean } -> Args -> Aff Unit
rebuildWith { bundleFileName, quiet } args =
  Build.build (args { commandOpts = addExtras args.commandOpts })
  where
  addExtras =
    Map.insert "to" (Just (unsafeToForeign bundleFileName))
    >>> if quiet then Map.insert "_silenced" Nothing else identity
