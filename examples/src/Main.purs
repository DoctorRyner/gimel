module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub, execEvent, logModel, activeWhen)
import Gimel.Sub.Time (every)
import Gimel.Sub.Window (getWindowSize, windowResize)
import Gimel.Types (Update)

data Event
  = IncrementCounter
  | DecrementCounter
  | OnWindowResize {height :: Int, width :: Int}

type Model =
  { counter :: Int
  , window  :: {height :: Int, width :: Int}
  }

init :: Effect Model
init = pure
  { counter: 0
  , window: {height: 0, width: 0}
  }

view :: Model -> Html Event
view model = fold
  [ button [onClick IncrementCounter] [text "+"]
  , textS model.counter
  , button [onClick DecrementCounter] [text "-"]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  OnWindowResize window -> pure model {window = window}
  IncrementCounter      -> pure model {counter = model.counter + 1}
  DecrementCounter      -> pure model {counter = model.counter - 1}

subs :: Array (Sub Model Event)
subs =
  [ logModel
  , execEvent IncrementCounter
  , getWindowSize OnWindowResize
  , activeWhen (\model -> model.counter < 5) $ windowResize OnWindowResize
  , every 1.0 IncrementCounter
  ]

main :: Effect Unit
main = run {init, view, update, subs}
