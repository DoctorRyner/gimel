module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Gimel.Attributes (onClick)
import Gimel.Cmd (cmd)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub, none)
import Gimel.Types (Update)
import Gimel.Utils (wait, withCmd)

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
  Dec ->
    withCmd
      (model - 1)
      (cmd do
        wait 1.0
        logShow $ model - 1
      )

subs :: Sub Model Event
subs = none

main :: Effect Unit
main = run {init, view, update, subs}
