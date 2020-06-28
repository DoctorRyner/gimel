module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Engine (run)
import Gimel.Html (Html, h1, text)
import Gimel.Sub (Sub, connect)
import Gimel.Types (Update, UpdateM(..))
import Gimel.Utils (withCmds)

import Component.Counter as Counter

data Event = Counter Counter.Event

type Model =
  { counter :: Counter.Model
  }

init :: Model
init =
  { counter: 0
  }

view :: Model -> Html Event
view model = fold
  [ h1 [] [text "Counter Component"]
  , map Counter $ Counter.view model.counter
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Counter event ->
    let Update counter = Counter.update model.counter event
     in withCmds
        (model {counter = counter.model})
        (map (map Counter) counter.cmds)

subs :: Sub Model Event
subs = connect (\model -> model.counter) Counter Counter.subs

main :: Effect Unit
main = run {init, view, update, subs}
