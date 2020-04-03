module Gimel.Types where

import Prelude

import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Gimel.Html (Html)

type Application model event =
  { init   :: Tuple model (Array (Aff event))
  , view   :: model -> Html event
  , update :: model -> event -> Tuple model (Array (Aff event))
  }

type Dispatcher event = event -> Effect Unit

type State model = { model :: model }

type Update model event = Tuple model (Array (Aff event))
