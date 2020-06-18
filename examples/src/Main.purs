module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub, execEvent, logModel, useWhen)
import Gimel.Sub.Time (every)
import Gimel.Sub.Window (getWindow, resizeWindow)
import Gimel.Types (Update)

data Event
  = IncrementCounter
  | DecrementCounter
  | OnResizeWindow {height :: Int, width :: Int}

type Model =
  { counter :: Int
  , window  :: {height :: Int, width :: Int}
  }

initialModel :: Model
initialModel =
  { counter: 0
  , window: {height: 0, width: 0}
  }

init :: Update Model Event
init = pure initialModel

view :: Model -> Html Event
view model = fold
  [ button [onClick IncrementCounter] [text "+"]
  , textS model.counter
  , button [onClick DecrementCounter] [text "-"]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  OnResizeWindow window -> pure model {window = window}
  IncrementCounter      -> pure model {counter = model.counter + 1}
  DecrementCounter      -> pure model {counter = model.counter - 1}

subs :: Array (Sub Model Event)
subs =
  [ logModel
  , execEvent IncrementCounter
  , getWindow OnResizeWindow
  , useWhen (\model -> model.counter < 5) $ resizeWindow OnResizeWindow
  , every 1.0 IncrementCounter
  ]

main :: Effect Unit
main = run {init, view, update, subs}
