module Gimel.Sub where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth, toEventTarget)

type Subs event = Array (Sub event)

data Sub event
  = Sub { id     :: String
        , attach :: (event -> Effect Unit) -> Effect (Effect Unit)
        }
  | SubSimple ((event -> Effect Unit) -> Effect Unit)

setSubId :: forall event. String -> Sub event -> Sub event
setSubId id (Sub s) = Sub s { id = id }
setSubId _ s        = s

resizeWindow :: forall event. (Int -> Int -> event) -> Sub event
resizeWindow resizeEvent = Sub
  { id    : "resizeWindow"
  , attach: \runEvent -> do
      win      <- window
      listener <-
        eventListener \_ -> do
          height <- innerHeight win
          width  <- innerWidth win

          runEvent $ resizeEvent height width

      addEventListener (EventType "resize") listener false $ toEventTarget win

      pure do
        removeEventListener
          (EventType "resize")
          listener
          false
          (toEventTarget win)
  }

logModel :: forall event model. Show model => model -> Sub event
logModel model = SubSimple \_ -> logShow model