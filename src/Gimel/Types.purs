module Gimel.Types where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Gimel.Html (Html)
import Gimel.Sub (Sub)

type Application model event =
  { init   :: model
  , view   :: model -> Html event
  , update :: model -> event -> Update model event
  , subs   :: Array (Sub model event)
  }

type Update model event = UpdateM event model

newtype UpdateM event model =
  Update
    { model :: model
    , affs  :: Array (Aff (Maybe event))
    }

instance functorUpdate :: Functor (UpdateM event) where
  map f (Update context) = Update context {model = f context.model}

instance applyUpdate :: Apply (UpdateM event) where
  apply (Update context) upd = context.model <$> upd

instance applicativeUpdate :: Applicative (UpdateM event) where
  pure model = Update {model, affs: []}

instance bindUpdate :: Bind (UpdateM event) where
  bind (Update context) f = f context.model

instance monadUpdate :: Monad (UpdateM event)

viewNone :: forall model event. model -> Html event
viewNone _ = fold []

updateNone :: forall model event. model -> event -> Update model event
updateNone model = pure <<< const model

subsNone :: forall model event. Array (Sub model event)
subsNone = []

mkApp :: forall model event. model -> Application model event
mkApp init = {init, view: viewNone, update: updateNone, subs: subsNone}

-- modifyModel :: forall model event. (model -> Aff model) -> Update model event
-- modifyModel f = Update {}