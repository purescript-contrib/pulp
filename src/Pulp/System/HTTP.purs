module Pulp.System.HTTP where

import Prelude
import Control.Monad.Aff
import Data.Maybe
import Data.Options (Options)
import Node.Buffer (Buffer)
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
