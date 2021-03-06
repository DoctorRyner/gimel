module Gimel.Cmd where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

newtype Cmd event = Cmd ((event -> Aff Unit) -> Aff Unit)

instance functorCmd :: Functor Cmd where
  map f (Cmd runCmd) = Cmd \runEvent -> runCmd $ runEvent <<< f

execEventCmd :: forall event. event -> Cmd event
execEventCmd event = execEventsCmd [event]

execEventsCmd :: forall event. Array event -> Cmd event
execEventsCmd events = Cmd \runEvent -> traverse_ runEvent events

cmd :: forall event. Aff Unit -> Cmd event
cmd f = Cmd \_ -> f

cmdEff :: forall event. Effect Unit -> Cmd event
cmdEff = cmd <<< liftEffect