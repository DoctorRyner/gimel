module Gimel.Sub where

import Prelude

import Effect (Effect)

type Subs event = Array (Sub event)
type Sub  event = (event -> Effect Unit) -> Effect Unit

mapSub :: forall eventA eventB. (eventA -> eventB) -> Sub eventA -> Sub eventB
mapSub f sub = \runB -> let runA = runB <<< f
                         in sub runA