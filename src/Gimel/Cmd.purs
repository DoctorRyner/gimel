module Gimel.Cmd where

import Prelude

import Data.Foldable (traverse_)
import Effect.Aff (Aff)

newtype Cmd event = Cmd ((event -> Aff Unit) -> Aff Unit)

execEventCmd :: forall event. event -> Cmd event
execEventCmd event = execEventsCmd [event]

execEventsCmd :: forall event. Array event -> Cmd event
execEventsCmd events = Cmd \runEvent -> traverse_ runEvent events