module Gimel.Sub where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Class.Console (logShow)

data Sub model event
  = Once   (model -> (event -> Effect Unit) -> Effect Unit)
  | Always (model -> (event -> Effect Unit) -> Effect Unit)
  | When (WhenSub model event)

type SubRef model event =
  Array
    (Either
      { condition :: model -> Boolean
      , attach    :: model -> (event -> Effect Unit) -> Effect (Effect Unit)
      }
      { condition :: model -> Boolean
      , attach    :: model -> (event -> Effect Unit) -> Effect (Effect Unit)
      , detach    :: Effect Unit
      }
    )

type WhenSub model event =
  { condition :: model -> Boolean
  , attach    :: model -> (event -> Effect Unit) -> Effect (Effect Unit)
  }

useWhen :: forall model event. (model -> Boolean) -> Sub model event -> Sub model event
useWhen condition (When x) = When x {condition = condition}
useWhen _ x                = x

logModel :: forall model event. Show model => Sub model event
logModel = Always \model _ -> logShow model

execEvent :: forall model event. event -> Sub model event
execEvent event = Once \_ runEvent -> runEvent event