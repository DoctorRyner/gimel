module Gimel.Types where

import Prelude

import Effect.Aff (Aff)
import Gimel.Html (Html)
import Data.Maybe (Maybe)
import Gimel.Sub (Sub)

type Application model event =
  { init   :: Update model event
  , view   :: model -> Html event
  , update :: model -> event -> Update model event
  , subs   :: Array (Sub model event)
  }

type Update model event = UpdateM event model

newtype UpdateM event model = Update { model :: model
                                     , affs :: Array (Aff (Maybe event))
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
