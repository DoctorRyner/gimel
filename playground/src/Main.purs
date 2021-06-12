module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS, withHooks)
import Gimel.Sub (Sub, none)
import Gimel.Types (Update)

data Event = Inc | Dec

type Model = {counter :: Int}

init :: Model
init = {counter: 0}

view :: Model -> Html Event
view model = withHooks do
  -- Hooks are WIP
  -- location <- useLocation

  pure $ fold
    [ button [onClick Inc] [text "+"]
    , textS model
    , button [onClick Dec] [text "-"]
    ]

update :: Model -> Event -> Update Model Event
update model event = case event of
  Inc -> pure model {counter = model.counter + 1}
  Dec -> pure model {counter = model.counter - 1}

subs :: Sub Model Event
subs = none

main :: Effect Unit
main = run {init, view, update, subs}
