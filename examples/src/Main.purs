module Main where

import Prelude hiding (div)

import Component.Counter as Counter
import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Component (Component, display, new, relay)
import Gimel.Engine (run)
import Gimel.Html (Html, h1, text)
import Gimel.Sub (Sub, connect)
import Gimel.Types (Update)

data Event = Counter1 Counter.Event | Counter2 Counter.Event

type Model =
  { counter1 :: Component Counter.Model Counter.Event Event
  , counter2 :: Component Counter.Model Counter.Event Event
  }

init :: Model
init =
  { counter1: new Counter1 Counter.app
  , counter2: new Counter2 Counter.app
  }

view :: Model -> Html Event
view model = fold
  [ h1 [] [text "Counter Component"]
  , display model.counter1
  , display model.counter2
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Counter1 event -> relay event (\x -> model {counter1 = x}) model.counter1
  Counter2 event -> relay event (\x -> model {counter2 = x}) model.counter2

subs :: Sub Model Event
subs = fold
  [ connect (\model -> model.counter1.app.init) Counter1 Counter.subs
  , connect (\model -> model.counter2.app.init) Counter2 Counter.subs
  ]

main :: Effect Unit
main = run {init, view, update, subs}
