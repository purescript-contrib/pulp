module Pulp.System.StaticServer where

import Prelude
import Node.HTTP as HTTP
import Pulp.System.FFI (EffN)

foreign import data StaticServer :: Type

-- | Create a static file server, given a base directory to serve files from.
foreign import new :: String -> EffN StaticServer

-- | Serve files; intended to be used within the callback to Node.HTTP.createServer.
foreign import serve :: StaticServer -> HTTP.Request -> HTTP.Response -> EffN Unit

-- | Serve a specific file; intended to be used within the callback to Node.HTTP.createServer.
-- | The `String` and `Int` arguments are the file path and status code respectively.
foreign import serveFile :: StaticServer -> String -> Int -> HTTP.Request -> HTTP.Response -> EffN Unit
