module Gimel.Dispatcher where

import Prelude

import Effect (Effect)

type Dispatcher event = event -> Effect Unit
