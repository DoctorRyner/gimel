module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (onClick)
import Gimel.Engine (run)
import Gimel.Html (Html, button, text, textS)
import Gimel.Sub (Sub, logModel, resizeWindow)
import Gimel.Types (Update)
import Gimel.Utils (wait, withAff, withEvents)

data Event
  = IncrementCounter
  | DecrementCounter
  | IncrementCounterEverySecond
  | Combine (Array Event)
  | OnResizeWindow Int Int

type Model = {counter :: Int, window :: {height :: Int, width :: Int}}

initialModel :: Model
initialModel = {counter: 0, window: {height: 0, width: 0}}

init :: Update Model Event
init = pure initialModel -- `withEvent` IncrementCounterEverySecond

view :: Model -> Html Event
view model = fold
  [ button [onClick IncrementCounter] [text "+"]
  , textS model.counter
  , button [onClick DecrementCounter] [text "-"]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  OnResizeWindow height width   -> pure model {window = {height, width}}
  IncrementCounter            -> pure model {counter = model.counter + 1}
  DecrementCounter            -> pure model {counter = model.counter - 1}
  Combine events              -> model `withEvents` events
  IncrementCounterEverySecond -> model `withAff` do wait 1.0 $> Combine [ IncrementCounter
                                                                        , IncrementCounterEverySecond
                                                                        ]

subs :: Model -> Array (Sub Event)
subs model =
  [ logModel model
  ]
    <>
      if model.counter <= 5
      then [resizeWindow OnResizeWindow]
      else []

main :: Effect Unit
main = run {init, view, update, subs}
