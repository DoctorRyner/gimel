module Gimel.Utils where

import Prelude

import Effect.Aff (Aff)
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
