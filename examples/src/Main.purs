module Main where

import Prelude hiding (div)

import Component.Counter as Counter
import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Engine (run)
import Gimel.Html (Html, h1, text)
import Gimel.Sub (Sub, none)
import Gimel.Types (Update, UpdateM(..))
import Gimel.Utils (withCmds)

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
  [ h1 [] [text "Counters:"]
  , Counter <$> Counter.view model.counter
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Counter event ->
    let Update next = Counter.update model.counter event
     in withCmds
        (model { counter = next.model })
        (map Counter <$> next.cmds)

subs :: Sub Model Event
subs = map Counter Counter.subs

main :: Effect Unit
main = run {init, view, update, subs}
