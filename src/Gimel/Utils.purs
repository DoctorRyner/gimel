module Gimel.Utils where

import Prelude

import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Gimel.Types (UpdateM(..), Update)

withEvent :: forall model event. model -> event -> Update model event
withEvent model event = withEvents model [event]

withEvents :: forall model event. model -> Array event -> Update model event
withEvents model affs = Update {model, affs: map (pure <<< Just) affs}

withAff :: forall model event. model -> Aff event -> Update model event
withAff model aff = withAffs model [aff]

withAffs :: forall model event. model -> Array (Aff event) -> Update model event
withAffs model = withMaybeAffs model <<< map (map Just)

withMaybeAffs :: forall model event. model -> Array (Aff (Maybe event)) -> Update model event
withMaybeAffs model affs = Update {model, affs}

withEffect :: forall model event. model -> Effect event -> Update model event
withEffect model aff = Update { model, affs: [Just <$> liftEffect aff] }

withEffects :: forall model event. model -> Array (Effect event) -> Update model event
withEffects model = withMaybeEffects model <<< map (map Just)

withMaybeEffects :: forall model event. model -> Array (Effect (Maybe event)) -> Update model event
withMaybeEffects model = withMaybeAffs model <<< map liftEffect

wait :: Number -> Aff Unit
wait sec = delay $ Milliseconds $ sec * 1000.0

justs :: forall a. Array (Maybe a) -> Array a
justs [] = []
justs xs =
  case uncons xs of
    Just {head: x, tail} ->
      case x of
        Just val -> [val] <> justs tail
        Nothing  -> justs tail
    Nothing              -> []