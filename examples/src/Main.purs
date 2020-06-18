module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub, logModel, subIf)
import Gimel.Sub.Time (every)
import Gimel.Sub.Window (resizeWindow)
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
initialModel = {counter: 0, window: {height: 0, width: 0}}

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

subs :: Model -> Array (Sub Event)
subs model =
  [ logModel model
  , every 1.0 IncrementCounter
  , subIf (model.counter <= 5) $ resizeWindow OnResizeWindow
  ]

main :: Effect Unit
main = run {init, view, update, subs}
