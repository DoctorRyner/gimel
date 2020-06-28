module Component.Counter where

import Prelude

import Data.Foldable (fold)
import Effect.Class.Console (logShow)
import Gimel.Attributes (onClick)
import Gimel.Cmd (cmd, execEventCmd)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub)
import Gimel.Sub.Time (every)
import Gimel.Types (Update)
import Gimel.Utils (withCmd)

data Event = Inc | Dec

type Model = Int

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
      (cmd $ logShow model)

subs :: Sub Model Event
subs = every 1.0 $ execEventCmd Inc