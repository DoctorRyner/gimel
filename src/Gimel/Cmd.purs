module Gimel.Cmd where

import Prelude

import Effect.Aff (Aff)

newtype Cmd event = Cmd ((event -> Aff Unit) -> Aff Unit)