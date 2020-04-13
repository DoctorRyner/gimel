module Main where

import Gimel.Html
import Gimel.Types
import Prelude hiding (div)
import Types

import Effect (Effect)
import Effect.Console (log)
import Gimel.Engine (run)
import Gimel.EventRunner

main :: Effect Unit
main = run {init, subs, update, view}

view :: Model -> Html Event
view model = div [] [text "Hello worudo!"]

update :: Model -> Event -> Update Model Event 
update model event = case event of 
    NoEvent -> pure model

subs :: Model -> EventRunner Event -> Array (Effect Unit)
subs model event = 
    [
    ]

init :: Update Model Event
init = pure
    {
    }