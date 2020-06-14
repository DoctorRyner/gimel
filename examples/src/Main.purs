module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Effect.Console (logShow)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Subs)
import Gimel.Types (Update)
import Gimel.Utils (wait, withAff, withEvent, withEvents)

data Event
  = IncrementCounter
  | DecrementCounter
  | IncrementCounterEverySecond
  | Combine (Array Event)

type Model = {counter :: Int}

initialModel :: Model
initialModel = {counter: 0}

init :: Update Model Event
init = initialModel `withEvent` IncrementCounterEverySecond

view :: Model -> Html Event
view model = fold
  [ button [onClick IncrementCounter] [text "+"]
  , textS model.counter
  , button [onClick DecrementCounter] [text "-"]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  IncrementCounter            -> pure model {counter = model.counter + 1}
  DecrementCounter            -> pure model {counter = model.counter - 1}
  Combine events              -> model `withEvents` events
  IncrementCounterEverySecond -> model `withAff` do wait 1.0 $> Combine [ IncrementCounter
                                                                        , IncrementCounterEverySecond
                                                                        ]

subs :: Model -> Subs Event
subs model = [\_ -> logShow model]

main :: Effect Unit
main = run {init, view, update, subs}
