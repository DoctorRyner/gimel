module Gimel.Sub.Window where

import Prelude

import Gimel.Sub (Sub, mkSubEff)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth, toEventTarget)

windowResize :: forall model event. ({height :: Int, width :: Int} -> event) -> Sub model event
windowResize resizeEvent = mkSubEff \_ runEvent -> do
  win      <- window
  listener <-
    eventListener \_ -> do
      height <- innerHeight win
      width  <- innerWidth win

      runEvent $ resizeEvent {height, width}

  addEventListener (EventType "resize") listener false $ toEventTarget win

  pure $
    removeEventListener
      (EventType "resize")
      listener
      false
      (toEventTarget win)

getWindowSize :: forall model event. ({height :: Int, width :: Int} -> event) -> Sub model event
getWindowSize getWindowEvent = mkSubEff \_ runEvent -> do
  win    <- window
  height <- innerHeight win
  width  <- innerWidth win

  runEvent $ getWindowEvent {height, width}
  pure mempty