module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Effect.Console (log)
import Gimel.Attributes (onClick)
import Gimel.Dispatcher (Dispatcher)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Types (Update)
import Gimel.Utils (wait, withAff, withEvent, withEvents)

data Event
  = Inc
  | Dec
  | Batch (Array Event)
  | IncEachSecond

type Model = { counter :: Int }

initialModel :: Model
initialModel = { counter: 0 }

view :: Model -> Html Event
view model = fold
  [ button [onClick Inc] [text "+"]
  , textS model.counter
  , button [onClick Dec] [text "-"]
  ]

update :: Model -> Event -> Update Event Model
update model = case _ of
  Inc           -> pure model { counter = model.counter + 1 }
  Dec           -> pure model { counter = model.counter - 1 }
  Batch events  -> model `withEvents` events
  IncEachSecond -> withAff model $ wait 1 $> Batch [Inc, IncEachSecond]

subs :: Model -> Dispatcher Event -> Array (Effect Unit)
subs model runEvent =
  [ log "kek"
  ]

main :: Effect Unit
main = run
  { init: initialModel `withEvent` IncEachSecond
  , view
  , update
  , subs
  }
