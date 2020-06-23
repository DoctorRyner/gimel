module Gimel.Http where

import Prelude

import Affjax (URL, printError)
import Affjax as Affjax
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat (json)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Class.Console (error)
import Gimel.Cmd (Cmd(..))

post'
  :: forall body result event
  .  EncodeJson body
  => DecodeJson result
  => URL
  -> Maybe body
  -> Array (RequestHeader)
  -> (result -> event)
  -> Cmd event
post' url body headers receivingEvent = Cmd \runEvent -> do
  res <- toResult =<< Affjax.request
    Affjax.defaultRequest
      { responseFormat = json
      , url = url
      , method = Left POST
      , content = Json <<< encodeJson <$> body
      , headers = headers
      }

  maybe mempty (runEvent <<< receivingEvent) res

post
  :: forall body result event
  .  EncodeJson body
  => DecodeJson result
  => URL
  -> body
  -> (result -> event)
  -> Cmd event
post url body receivingEvent = post' url (Just body) [] receivingEvent

get'
  :: forall body result event
  .  EncodeJson body
  => DecodeJson result
  => URL
  -> Array (RequestHeader)
  -> (result -> event)
  -> Cmd event
get' url headers receivingEvent = Cmd \runEvent -> do
  res <- toResult =<< Affjax.request
    Affjax.defaultRequest
      { responseFormat = json
      , url = url
      , headers = headers
      }

  maybe mempty (runEvent <<< receivingEvent) res

get
  :: forall body result event
  .  EncodeJson body
  => DecodeJson result
  => URL
  -> (result -> event)
  -> Cmd event
get url receivingEvent = Cmd \runEvent -> do
  res <- toResult =<< Affjax.request
    Affjax.defaultRequest
      { responseFormat = json
      , url = url
      , headers = []
      }

  maybe mempty (runEvent <<< receivingEvent) res

toResult
  :: forall result
  .  DecodeJson result
  => Either Affjax.Error (Affjax.Response Json)
  -> Aff (Maybe result)
toResult = case _ of
  Left err -> error (printError err) $> Nothing
  Right resp ->
    case decodeJson resp.body of
      Left errStr -> error errStr $> Nothing
      Right result -> pure $ Just result