module Gimel.Utils where

import Prelude

import CSS (CSS, fromString, prefixed, render, renderedInline)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Gimel.Cmd (Cmd(..))
import Gimel.Types (UpdateM(..), Update)

withEvent :: forall model event. model -> event -> Update model event
withEvent model event = withEvents model [event]

withEvents :: forall model event. model -> Array event -> Update model event
withEvents model events =
  Update
    { model
    , cmds: map (\event -> Cmd \runEvent -> runEvent event)
                events
    }

withAff :: forall model event. model -> Aff event -> Update model event
withAff model aff = withAffs model [aff]

withMaybeAff :: forall model event. model -> Aff (Maybe event) -> Update model event
withMaybeAff model aff = withMaybeAffs model [aff]

withAffs :: forall model event. model -> Array (Aff event) -> Update model event
withAffs model = withMaybeAffs model <<< map (map Just)

withMaybeAffs :: forall model event. model -> Array (Aff (Maybe event)) -> Update model event
withMaybeAffs model affs =
  Update
    { model
    , cmds: map (\aff -> Cmd \runEvent -> maybe mempty runEvent =<< aff)
                affs
    }

withEff :: forall model event. model -> Effect event -> Update model event
withEff model eff = withEffs model [eff]

withMaybeEff :: forall model event. model -> Effect (Maybe event) -> Update model event
withMaybeEff model eff = withMaybeEffs model [eff]

withEffs :: forall model event. model -> Array (Effect event) -> Update model event
withEffs model = withMaybeEffs model <<< map (map Just)

withMaybeEffs :: forall model event. model -> Array (Effect (Maybe event)) -> Update model event
withMaybeEffs model = withMaybeAffs model <<< map liftEffect

withCmds :: forall model event. model -> Array (Cmd event) -> Update model event
withCmds model cmds = Update {model, cmds}

withCmd :: forall model event. model -> Cmd event -> Update model event
withCmd model cmd = withCmds model [cmd]

wait :: Number -> Aff Unit
wait sec = delay $ Milliseconds $ sec * 1000.0

custom :: String -> String -> CSS
custom = prefixed <<< fromString

infix 4 custom as |:

renderCSS :: CSS -> String
renderCSS = fromMaybe "<!INVALID-CSS!>" <<< renderedInline <<< render