module Gimel.Types where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Effect (Effect)
import Effect.Aff (Aff)
import Gimel.EventRunner (EventRunner)
import Gimel.Html (Html)

type Application model event =
  { init   :: Update model event
  , view   :: model -> Html event
  , update :: model -> event -> Update model event
  , subs   :: model -> EventRunner event -> Array (Effect Unit)
  }

type Update model event = UpdateM event model

newtype UpdateM event model = Update { model :: model, affs :: Array (Aff event) }

instance bifunctorUpdate :: Bifunctor UpdateM where
  bimap g f (Update context) = Update context { model = f context.model, affs = (map g) <$> context.affs }

instance functorUpdate :: Functor (UpdateM event) where
  map f (Update context) = Update context { model = f context.model }

instance applyUpdate :: Apply (UpdateM event) where
  apply (Update context) upd = context.model <$> upd

instance applicativeUpdate :: Applicative (UpdateM event) where
  pure model = Update { model, affs: [] }

instance bindUpdate :: Bind (UpdateM event) where
  bind (Update context) f = f context.model

instance monadUpdate :: Monad (UpdateM event)
