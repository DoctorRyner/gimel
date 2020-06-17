module Gimel.Sub where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)

data Sub event
  = Sub { id     :: String
        , attach :: (event -> Effect Unit) -> Effect (Effect Unit)
        }
  | SubSimple ((event -> Effect Unit) -> Effect Unit)
  | SubNone

setSubId :: forall event. String -> Sub event -> Sub event
setSubId id (Sub s) = Sub s { id = id }
setSubId _ s        = s

subIf :: forall event. Boolean -> Sub event -> Sub event
subIf cond sub = if cond then sub else SubNone

logModel :: forall event model. Show model => model -> Sub event
logModel model = SubSimple \_ -> logShow model