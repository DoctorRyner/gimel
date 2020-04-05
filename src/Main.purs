module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)

import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button_, text)
import Gimel.Types (Update)

data Event = Inc | Dec

type Model = { counter :: Int }

initialModel :: Model
initialModel = { counter: 0 }

view :: Model -> Html Event
view model = fold
  [ text $ show model.counter
  , button_ [onClick Inc] $ text "+"
  , button_ [onClick Dec] $ text "-"
  ]

update :: Model -> Event -> Update Event Model
update model = case _ of
  Inc -> pure model { counter = model.counter + 1 }
  Dec -> pure model { counter = model.counter - 1 }

main :: Effect Unit
main = run
  { init: pure initialModel
  , view
  , update
  }

