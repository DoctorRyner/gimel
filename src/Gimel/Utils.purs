module Gimel.Utils where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Gimel.Types (UpdateM(..), Update)

infix 4 withEvent   as <<
infix 4 withEvents  as <:
infix 4 withAff     as <~
infix 4 withAffs    as <~:
infix 4 withEffect  as <|
infix 4 withEffects as <|:

withEvent :: forall model event. model -> event -> Update model event
withEvent model event = Update { model, affs: [pure event] }

withEvents :: forall model event. model -> Array event -> Update model event
withEvents model affs = Update { model, affs: map pure affs }

withAff :: forall model event. model -> Aff event -> Update model event
withAff model aff = Update { model, affs: [aff] }

withAffs :: forall model event. model -> Array (Aff event) -> Update model event
withAffs model affs = Update { model, affs }

withEffect :: forall model event. model -> Effect event -> Update model event
withEffect model aff = Update { model, affs: [liftEffect aff] }

withEffects :: forall model event. model -> Array (Effect event) -> Update model event
withEffects model affs = Update { model, affs: map liftEffect affs }

class Wait time where
  wait :: time -> Aff Unit

instance waitNumber :: Wait Number where
  wait sec = delay $ Milliseconds $ sec * 1000.0

instance waitInt :: Wait Int where
  wait sec = delay $ Milliseconds $ toNumber $ sec * 1000
