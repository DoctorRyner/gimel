module Gimel.Engine where

import Prelude

import Data.Either (either)
import Data.Foldable (fold, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (runAff_, Aff)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Gimel.Html (toReactHtml)
import Gimel.Types (Application, UpdateM(..))
import React (Children, ReactClass, ReactElement, createElement, getState, modifyState)
import React (component) as React
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

classFromApp :: forall event model. Application model event -> ReactClass { children :: Children | () }
classFromApp app = React.component "Gimel" constructor
 where
  constructor this = do
    let Update initial = app.init

    modelRef <- Ref.new initial.model

    let
      runEvent :: event -> Effect Unit
      runEvent event = do
        model <- Ref.read modelRef

        let Update next = app.update model event

        Ref.write next.model modelRef
        modifyState this $ \state -> state {model = next.model}
        runAffs next.affs

        -- Perform subscriptions
        fold $ (\f -> f runEvent) <$> app.subs next.model

      runMaybeEvent :: Maybe event -> Effect Unit
      runMaybeEvent x = maybe mempty runEvent x

      runAffs :: Array (Aff (Maybe event)) -> Effect Unit 
      runAffs affs = traverse_ (runAff_ $ either (log <<< show) runMaybeEvent) affs

      renderHtml :: {model :: model} -> ReactElement
      renderHtml state = toReactHtml runEvent $ app.view state.model

    pure
      { state: {model: initial.model}
      , componentDidMount: runAffs initial.affs
      , render: renderHtml <$> getState this
      }

run :: forall model event. Application model event -> Effect Unit
run = runOn "gimel"

runOn :: forall model event. String -> Application model event -> Effect Unit
runOn nodeId app = do
  win       <- DOM.window
  maybeRoot <- DOM.getElementById nodeId =<< DOM.toNonElementParentNode <$> DOM.document win

  case maybeRoot of
    Just root -> render (createElement (classFromApp app) {} []) root *> mempty
    Nothing   -> errorShow $ "Can't find an element with an id " <> nodeId
