module Gimel.Sub.Time where

import Prelude

import Data.Int (round)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Gimel.Sub (Sub(..))
import Gimel.Utils (wait)

-- every :: forall event. Number -> event -> Sub event
-- every offset timeEvent = Sub
--   { id: "every"
--   , attach: \runEvent ->
--       clearInterval
--         <$> setInterval (round $ offset * 1000.0) (runEvent timeEvent)
--   }

every :: forall model event. Number -> event -> Sub model event
every offset timeEvent =
  When
    { condition: const true
    , attach: \_ runEvent ->
        clearInterval
          <$> setInterval (round $ offset * 1000.0) (runEvent timeEvent)
    }

delay :: forall model event. Number -> event -> Sub model event
delay offset timeEvent = Once
  \_ runEvent -> launchAff_ do
    wait offset
    liftEffect $ runEvent timeEvent
