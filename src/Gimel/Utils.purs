module Gimel.Utils where

import Prelude

import Data.Int (toNumber)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Gimel.Types (Update(..))

infix 4 withEvent  as <<
infix 4 withEvents as <:
infix 4 withAff    as <~
infix 4 withAffs   as <~:

withEvent :: forall model event. model -> event -> Update event model
withEvent model event = Update model [pure event]

withEvents :: forall model event. model -> Array event -> Update event model
withEvents model = Update model <<< map pure

withAff :: forall model event. model -> Aff event -> Update event model
withAff model aff = Update model [aff]

withAffs :: forall model event. model -> Array (Aff event) -> Update event model
withAffs = Update

class Wait time where
  wait :: time -> Aff Unit

instance waitNumber :: Wait Number where
  wait sec = delay $ Milliseconds $ sec * 1000.0

instance waitInt :: Wait Int where
  wait sec = delay $ Milliseconds $ toNumber $ sec * 1000
