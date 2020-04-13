module Main where

import Gimel.EventRunner
import Gimel.Html hiding (style)
import Gimel.Types
import Gimel.Attributes
import Gimel.Utils
import Prelude hiding (div)
import Types

import Effect (Effect)
import Effect.Console (log)
import Gimel.Engine (run)

main :: Effect Unit
main = run {init, subs, update, view}

view :: Model -> Html Event
view model = div []
    [ div --Elements with css style
        [ style
            { display: "flex"
            , flexDirection: "row"
            , justifyContent: "center"
            , alignItems: "center"
            , height: "500px"
            , width: "500px"
            }
        ]

        [ button [onClick IncN, style {width: "30px", height: "30px"}] [text "+"]
        , h2 []  [textS model.number]
        , button [onClick DecN, style {width: "30px", height: "30px"}] [text "-"]
        , button [onClick $ Combine [IncN, Print "Inc by 1, then print"]] [text "Combine event example"]
        ]

    , div --Widgets
        []
        [

        ]
    ]

update :: Model -> Event -> Update Model Event 
update model event = case event of 
    IncN -> pure model { number = model.number + 1 }
    DecN -> pure model { number = model.number - 1 }
    Print text -> withEffect model $ log text $> NoEvent
    Combine events -> withEvents model events

    NoEvent -> pure model

subs :: Model -> EventRunner Event -> Array (Effect Unit)
subs model event =
    [
    ]

init :: Update Model Event
init = pure
    { text: "Hello worudo"
    , number: 0
    }