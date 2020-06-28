module Gimel.Component where

import Prelude

import Gimel.Html (Html)
import Gimel.Types (Application, UpdateM(..), Update)
import Gimel.Utils (withCmds)

type Component model event1 event2 =
  { transformMsg :: event1 -> event2
  , app :: Application model event1
  }

new
  :: forall model event1 event2
  .  (event1 -> event2)
  -> Application model event1
  -> Component model event1 event2
new transformMsg app = {transformMsg, app}

display :: forall model event1 event2. Component model event1 event2 -> Html event2
display component = map component.transformMsg $ component.app.view component.app.init

relay
  :: forall model parentModel event1 event2
  .  event1
  -> (Component model event1 event2 -> parentModel)
  -> Component model event1 event2
  -> Update parentModel event2
relay event inject component =
  let Update next = component.app.update component.app.init event
   in withCmds
      (inject $ component {app = component.app {init = next.model}})
      (map (map component.transformMsg) next.cmds)
