module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub, enableWhen, execEvent)
import Gimel.Sub.Window (windowResize)
import Gimel.Types (Update)

data Event = Inc | Dec | OnWindowResize {height :: Int, width :: Int}

type Model = {counter :: Int, window :: {height :: Int, width :: Int}}

init :: Model
init = {counter: 0, window: {height: 0, width: 0}}

view :: Model -> Html Event
view model = fold
  [ button [onClick Inc] [text "+"]
  , textS model
  , button [onClick Dec] [text "-"]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Inc -> pure model {counter = model.counter + 1}
  Dec -> pure model {counter = model.counter - 1}
  OnWindowResize window -> pure model {window = window}

subs :: Array (Sub Model Event)
subs =
  [ enableWhen (\model -> model.counter < 5) $ windowResize OnWindowResize
  , enableWhen (\model -> model.counter == 5) $ execEvent Inc
  ]

main :: Effect Unit
main = run {init, view, update, subs}
