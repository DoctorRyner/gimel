module Gimel.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Gimel.Cmd (Cmd)
import Gimel.Types (UpdateM(..), Update)

withEvent :: forall model event. model -> event -> Update model event
withEvent model event = withEvents model [event]

withEvents :: forall model event. model -> Array event -> Update model event
withEvents model affs = Update {model, affs: map (pure <<< Just) affs, cmds: []}

withAff :: forall model event. model -> Aff event -> Update model event
withAff model aff = withAffs model [aff]

withMaybeAff :: forall model event. model -> Aff (Maybe event) -> Update model event
withMaybeAff model aff = withMaybeAffs model [aff]

withAffs :: forall model event. model -> Array (Aff event) -> Update model event
withAffs model = withMaybeAffs model <<< map (map Just)

withMaybeAffs :: forall model event. model -> Array (Aff (Maybe event)) -> Update model event
withMaybeAffs model affs = Update {model, affs, cmds: []}

withEffect :: forall model event. model -> Effect event -> Update model event
withEffect model aff = Update {model, affs: [Just <$> liftEffect aff], cmds: []}

withMaybeEffect :: forall model event. model -> Effect (Maybe event) -> Update model event
withMaybeEffect model aff = Update {model, affs: [liftEffect aff], cmds: []}

withEffects :: forall model event. model -> Array (Effect event) -> Update model event
withEffects model = withMaybeEffects model <<< map (map Just)

withMaybeEffects :: forall model event. model -> Array (Effect (Maybe event)) -> Update model event
withMaybeEffects model = withMaybeAffs model <<< map liftEffect

withCmds :: forall model event. model -> Array (Cmd event) -> Update model event
withCmds model cmds = Update {model, affs: [], cmds}

withCmd :: forall model event. model -> Cmd event -> Update model event
withCmd model cmd = withCmds model [cmd]

wait :: Number -> Aff Unit
wait sec = delay $ Milliseconds $ sec * 1000.0

log :: forall m. MonadEffect m => String -> m Unit
log = liftEffect <<< Console.log

error :: forall m. MonadEffect m => String -> m Unit
error = liftEffect <<< Console.log