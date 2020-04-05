module Gimel.Types where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Gimel.Html (Html)

type Application model event =
  { init   :: Update event model
  , view   :: model -> Html event
  , update :: model -> event -> Update event model
  }

type Dispatcher event = event -> Effect Unit

type State model = { model :: model }

data Update event model = Update model (Array (Aff event))

instance functorUpdate :: Functor (Update event) where
  map f (Update model affs) = Update (f model) affs

instance applyUpdate :: Apply (Update event) where
  apply (Update f _) upd = f <$> upd

instance applicativeUpdate :: Applicative (Update event) where
  pure model = Update model []

instance bindUpdate :: Bind (Update event) where
  bind (Update model _) f = f model

instance monadUpdate :: Monad (Update event)
