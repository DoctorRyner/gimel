module Main where

import Prelude hiding (div)

import Effect (Effect)
import Gimel.Engine (run)
import Gimel.Html (Html, text)
import Gimel.Sub (Sub, none)
import Gimel.Types (Update)

data Event = Inc | Dec

type Model = {counter :: Int}

init :: Model
init = {counter: 0}

view :: Model -> Html Event
view model = text "Hello, World!"

update :: Model -> Event -> Update Model Event
update model event = case event of
  Inc -> pure model {counter = model.counter + 1}
  Dec -> pure model {counter = model.counter - 1}

subs :: Sub Model Event
subs = none

main :: Effect Unit
main = run {init, view, update, subs}
