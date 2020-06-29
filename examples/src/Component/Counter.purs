module Component.Counter where

import Prelude

import Data.Foldable (fold)
import Effect.Class.Console (logShow)
import Gimel.Attributes (onClick)
import Gimel.Cmd (cmd, execEventCmd)
import Gimel.Html (Html, br, button, text, textS)
import Gimel.Sub (Sub, enableWhen)
import Gimel.Sub.Time (every)
import Gimel.Types (Update, Application)
import Gimel.Utils (withCmd)

data Event = Inc | Dec

type Model = Int

view :: Model -> Html Event
view model = fold
  [ button [onClick Inc] [text "+"]
  , textS model
  , button [onClick Dec] [text "-"]
  , br
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Inc -> pure $ model + 1
  Dec ->
    withCmd
      (model - 1)
      (cmd $ logShow model)

subs :: Sub Model Event
subs =
  -- enableWhen (\model -> model < 4) $
  every 1.0 (execEventCmd Inc)

app :: Application Model Event
app = {init: 0, view, update, subs}