module Gimel.Types where

import Prelude

import Data.Foldable (fold)
import Gimel.Cmd (Cmd)
import Gimel.Html (Html)
import Gimel.Sub (Sub, none)

type Application model event =
  { init   :: model
  , view   :: model -> Html event
  , update :: model -> event -> Update model event
  , subs   :: Sub model event
  }

type Update model event = UpdateM event model

newtype UpdateM event model =
  Update
    { model :: model
    , cmds  :: Array (Cmd event)
    }

instance Functor (UpdateM event) where
  map f (Update context) = Update context {model = f context.model}

instance Apply (UpdateM event) where
  apply (Update context) upd = context.model <$> upd

instance Applicative (UpdateM event) where
  pure model = Update {model, cmds: []}

instance Bind (UpdateM event) where
  bind (Update context) f = f context.model

instance Monad (UpdateM event)

viewNone :: forall model event. model -> Html event
viewNone _ = fold []

updateNone :: forall model event. model -> event -> Update model event
updateNone model = pure <<< const model

mkApp :: forall model event. model -> Application model event
mkApp init = {init, view: viewNone, update: updateNone, subs: none}