module Gimel.Sub.Time where

import Prelude

import Data.Int (round)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Gimel.Cmd (Cmd(..))
import Gimel.Sub (Sub, mkSub, mkSubEff)
import Gimel.Utils (wait)

every :: forall model event. Number -> Cmd event -> Sub model event
every offset (Cmd cmd) = mkSubEff \_ runEvent ->
  clearInterval <$> setInterval (round $ offset * 1000.0)
                                (launchAff_ $ cmd (liftEffect <<< runEvent))

delay :: forall model event. Number -> Cmd event -> Sub model event
delay offset (Cmd cmd) = mkSub \_ runEvent -> do
  wait offset
  cmd runEvent $> mempty
