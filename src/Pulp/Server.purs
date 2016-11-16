
module Pulp.Server
  ( action
  ) where

import Prelude
import Data.Maybe
import Data.Either
import Data.Map as Map
import Data.Foreign (toForeign)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (makeAff, launchAff, attempt)
import Control.Monad.Aff.AVar as AVar
import Node.HTTP as HTTP
import Node.Encoding (Encoding(..))
import Node.Stream as Stream

import Pulp.System.FFI
import Pulp.System.StaticServer as StaticServer
import Pulp.Outputter
import Pulp.Args
import Pulp.Args.Get
import Pulp.Watch (watchAff, watchDirectories)
import Pulp.Build as Build
import Pulp.Utils (orErr)

data BuildResult
  = Succeeded
  | Failed

getBundleFileName :: Options -> AffN String
getBundleFileName opts =
  (_ <> "/app.js") <$> getOption' "buildPath" opts

action :: Action
action = Action \args -> do
  let opts = Map.union args.globalOpts args.commandOpts
  out <- getOutputter args

  bundleFileName <- getBundleFileName opts
  hostname <- getOption' "host" opts
  port <- getOption' "port" opts

  -- This AVar should be 'full' (i.e. contain a value) if and only if the
  -- most recent build attempt has finished; in this case, the value inside
  -- tells you whether the build was successful and the bundle can be served
  -- to the client.
  rebuildV <- AVar.makeVar

  server <- liftEff $ createServer rebuildV bundleFileName
  listen server { hostname, port, backlog: Nothing }

  out.log $ "Server listening on http://" <> hostname <> ":" <> show port <> "/"

  quiet <- getFlag "quiet" opts
  let rebuild = do
        r <- attempt (rebuildWith { bundleFileName, quiet } args)
        case r of
          Right _ ->
            AVar.putVar rebuildV Succeeded
          Left _ -> do
            AVar.putVar rebuildV Failed
            out.err $ "Failed to rebuild; try to fix the compile errors"
  rebuild

  dirs <- watchDirectories opts >>= orErr "Internal error: unexpected Nothing"
  watchAff dirs \_ -> do
    void $ AVar.takeVar rebuildV
    rebuild

createServer :: AVar.AVar BuildResult -> String -> EffN HTTP.Server
createServer rebuildV bundleFileName = do
  static <- StaticServer.new "."
  HTTP.createServer \req res ->
    case (HTTP.requestURL req) of
      "/app.js" ->
        void $ unsafeToEffN $ launchAff do
          -- The effect of this line should be to block until the current
          -- rebuild is finished (if any).
          r <- AVar.peekVar rebuildV
          liftEff $ case r of
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

listen :: HTTP.Server -> HTTP.ListenOptions -> AffN Unit
listen server opts =
  -- TODO: error handling?
  makeAff \_ done -> HTTP.listen server opts (done unit)

rebuildWith :: { bundleFileName :: String, quiet :: Boolean } -> Args -> AffN Unit
rebuildWith { bundleFileName, quiet } args =
  Build.build (args { commandOpts = addExtras args.commandOpts })
  where
  addExtras =
    Map.insert "to" (Just (toForeign bundleFileName))
    >>> if quiet then Map.insert "_silenced" Nothing else id
