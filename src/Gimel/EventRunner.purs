module Gimel.EventRunner where

import Prelude
import Effect (Effect)

type EventRunner event = event -> Effect Unit
