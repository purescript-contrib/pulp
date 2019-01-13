module Pulp.System.HTTP where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Options (Options)
import Effect.Aff (Aff, makeAff)
import Node.Buffer (Buffer)
import Node.HTTP.Client as HTTP
import Node.Stream as Stream

httpRequest :: Options HTTP.RequestOptions -> Maybe Buffer -> Aff HTTP.Response
httpRequest reqOptions reqBody =
  makeAff \cb -> do
    req <- HTTP.request reqOptions (cb <<< Right)
    let reqStream = HTTP.requestAsStream req
    Stream.onError reqStream (cb <<< Left)
    maybeWrite reqStream reqBody do
      Stream.end reqStream do
        pure unit
    pure mempty
  where
  maybeWrite stream (Just body) next = void (Stream.write stream body next)
  maybeWrite _ Nothing next = next
