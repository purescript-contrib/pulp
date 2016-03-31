module Pulp.System.HTTP where

import Prelude
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.Error.Class
import Control.Monad.Aff
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested ((/\))
import Data.Either
import Data.Foldable (fold)
import Data.Foreign (Foreign, parseJSON)
import Data.Foreign.Class (readProp)
import Data.Version (Version)
import Data.Version as Version
import Data.String as String
import Data.StrMap as StrMap
import Data.Options (Options)
import Node.Encoding (Encoding(..))
import Node.Buffer (Buffer)
import Node.ChildProcess as CP
import Node.FS.Aff as FS
import Node.HTTP.Client as HTTP
import Node.Stream as Stream

import Pulp.System.FFI

httpRequest :: Options HTTP.RequestOptions -> Maybe Buffer -> AffN HTTP.Response
httpRequest reqOptions reqBody =
  makeAff \err done -> do
    req <- HTTP.request reqOptions done
    let reqStream = HTTP.requestAsStream req
    Stream.onError reqStream err
    maybeWrite reqStream reqBody do
      Stream.end reqStream do
        pure unit
  where
  maybeWrite stream (Just body) next = void (Stream.write stream body next)
  maybeWrite _ Nothing next = next
