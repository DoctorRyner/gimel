module Gimel.Sub where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Gimel.Cmd (Cmd(..))

data Sub model event
  = Once (model -> (event -> Effect Unit) -> Effect Unit)
  | Always (model -> (event -> Effect Unit) -> Effect Unit)
  | ActiveWhen (ActiveSubInstance model event)

type ActiveSubInstance model event =
  { check    :: model -> Boolean
  , activate :: model -> (event -> Effect Unit) -> Effect (Effect Unit)
  , status   :: ActiveSubStatus
  }

data ActiveSubStatus
  = Active {stop :: Effect Unit}
  | Inactive

mkActiveSub
  :: forall model event
  .  (model -> (event -> Effect Unit) -> Effect (Effect Unit))
  -> Sub model event
mkActiveSub activate = ActiveWhen {check: const true, activate, status: Inactive}

activeWhen :: forall model event. (model -> Boolean) -> Sub model event -> Sub model event
activeWhen check (ActiveWhen x) = ActiveWhen x {check = check}
activeWhen _ x                  = x

logModel :: forall model event. Show model => Sub model event
logModel = Always $ const <<< logShow

execEvents :: forall model event. Array event -> Sub model event
execEvents events = Once \_ runEvent -> traverse_ runEvent events

runCmd :: forall model event. Cmd event -> Sub model event
runCmd cmd = runCmds [cmd]

runCmds :: forall model event. Array (Cmd event) -> Sub model event
runCmds cmds = Once \_ runEvent -> do
  traverse_
    (\(Cmd cmd) -> launchAff_ $ cmd (liftEffect <<< runEvent))
    cmds

execEvent :: forall model event. event -> Sub model event
execEvent e = execEvents [e]