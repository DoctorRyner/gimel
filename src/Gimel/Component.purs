module Gimel.Component where

import Prelude

import Data.Array (cons)
import Data.Foldable (find, foldMap)
import Data.Maybe (Maybe(..))
import Gimel.Html (Html)
import Gimel.Types (Application, UpdateM(..), Update)
import Gimel.Utils (withCmds)

newtype Component model event1 event2 = Component
  { transformMsg :: event1 -> event2
  , app :: Application model event1
  }

instance Show model => Show (Component model event1 event2) where
  show x = "Component (" <> show (getModel x) <> ")"

getModel :: forall model event1 event2. Component model event1 event2 -> model
getModel (Component component) = component.app.init

new
  :: forall model event1 event2
  .  (event1 -> event2)
  -> Application model event1
  -> Component model event1 event2
new transformMsg app = Component {transformMsg, app}

display :: forall model event1 event2. Component model event1 event2 -> Html event2
display (Component component) = map component.transformMsg $ component.app.view component.app.init

relay
  :: forall model parentModel event1 event2
  .  event1
  -> (Component model event1 event2 -> parentModel)
  -> Component model event1 event2
  -> Update parentModel event2
relay event inject (Component component) =
  let Update next = component.app.update component.app.init event
   in withCmds
      (inject $ Component component {app = component.app {init = next.model}})
      (map (map component.transformMsg) next.cmds)

data Box model event1 event2 =
  Box
    { items :: Array (BoxItem model)
    , transformMsg :: event1 -> Int -> event2
    , app :: Application model event1
    , idRatio :: Int
    }

instance Show model => Show (Box model event1 event2) where
  show (Box x) = "Box (" <> show x.items <> ", idRatio: " <> show x.idRatio <> ")"

type BoxItem model =
  { id :: Int
  , model :: model
  }

newBox
  :: forall model event1 event2
  .  (event1 -> Int -> event2)
  -> Application model event1
  -> Box model event1 event2
newBox transformMsg app = Box {items: [], app, transformMsg, idRatio: 0}

displayBox :: forall model event1 event2. Box model event1 event2 -> Html event2
displayBox (Box box) =
  foldMap
    (\item ->
      map
        (\msg -> box.transformMsg msg item.id)
        (box.app.view item.model)
    )
    box.items

addItem :: forall model event1 event2. Box model event1 event2 -> Box model event1 event2
addItem (Box box) =
  Box box
    { items = cons {model: box.app.init, id: box.idRatio} box.items
    , idRatio = box.idRatio + 1
    }

relayBox
  :: forall model parentModel event1 event2
  .  event1
  -> Int
  -> (Box model event1 event2 -> parentModel)
  -> Box model event1 event2
  -> Update parentModel event2
relayBox event id inject (Box box) =
  let
    itemToUpdate =
      case find (\x -> x.id == id) box.items of
        Just item -> item
        Nothing   -> {id: 0, model: box.app.init}

    Update next = box.app.update itemToUpdate.model event
   in 
    withCmds
      (inject $ Box box
        { items =
            map
              (\item ->
                if item.id == id
                then item {model = next.model}
                else item
              )
              box.items
        }
      )
      (map (map (\msg -> box.transformMsg msg id)) next.cmds)