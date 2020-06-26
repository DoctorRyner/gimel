module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub, none)
import Gimel.Types (Update)

data Event = Inc | Dec

type Model = Int

init :: Model
init = 0

view :: Model -> Html Event
view model = fold
  [ button [onClick Inc] [text "+"]
  , textS model
  , button [onClick Dec] [text "-"]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Inc -> pure $ model + 1
  Dec -> pure $ model - 1

subs :: Sub Model Event
subs = none

main :: Effect Unit
main = run {init, view, update, subs}
