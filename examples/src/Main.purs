module Main where

import Prelude hiding (div)

import Component.Counter as Counter
import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Component (Box, Component, addItem, display, displayBox, getModel, new, newBox, relay, relayBox)
import Gimel.Engine (run)
import Gimel.Html (Html, br, button, h1, text)
import Gimel.Sub (Sub, connect, logModel)
import Gimel.Types (Update)

data Event
  = Counter1 Counter.Event
  | Counter2 Counter.Event
  | Counters Counter.Event Int
  | AddCounter

type Model =
  { counter1 :: Component Counter.Model Counter.Event Event
  , counter2 :: Component Counter.Model Counter.Event Event
  , counters :: Box Counter.Model Counter.Event Event
  }

init :: Model
init =
  { counter1: new Counter1 Counter.app
  , counter2: new Counter2 Counter.app
  , counters: newBox Counters Counter.app
  }

view :: Model -> Html Event
view model = fold
  [ h1 [] [text "2 Counter Components"]
  , display model.counter1
  , display model.counter2
  , h1 [] [text "Counters Box"]
  , button [onClick AddCounter] [text "Add counter"]
  , br
  , displayBox model.counters
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Counter1 event -> relay event (\x -> model {counter1 = x}) model.counter1
  Counter2 event -> relay event (\x -> model {counter2 = x}) model.counter2
  Counters event id -> relayBox event id (\x -> model {counters = x}) model.counters
  AddCounter -> pure model {counters = addItem model.counters}

subs :: Sub Model Event
subs = fold
  [ connect (\model -> getModel model.counter1) Counter1 Counter.subs
  , connect (\model -> getModel model.counter2) Counter2 Counter.subs
  , logModel
  ]

main :: Effect Unit
main = run {init, view, update, subs}
