module Gimel.Sub.Window where

import Prelude

import Gimel.Sub (Sub(..))
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth, toEventTarget)

resizeWindow :: forall event. ({height :: Int, width :: Int} -> event) -> Sub event
resizeWindow resizeEvent = Sub
  { id: "resizeWindow"
  , attach: \runEvent -> do
      win      <- window
      listener <-
        eventListener \_ -> do
          height <- innerHeight win
          width  <- innerWidth win

          runEvent $ resizeEvent {height, width}

      addEventListener (EventType "resize") listener false $ toEventTarget win

      pure do
        removeEventListener
          (EventType "resize")
          listener
          false
          (toEventTarget win)
  }