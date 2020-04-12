module Gimel.Engine where

import Prelude

import Data.Either (either)
import Data.Foldable (fold, traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Gimel.Html (toReactHtml)
import Gimel.Types (Application, UpdateM(..))
import React (Children, ReactClass, createElement, getState, modifyState)
import React (component) as React
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

-- TODO: add components support
-- component :: forall model event. Application model event -> Html event
-- component app = RawReact $ createElement (mkGimelApp app) {} []

classFromApp :: forall event model. Application model event -> ReactClass { children :: Children | () }
classFromApp app = React.component "Gimel" constructor
 where
  constructor this = do
    let Update initial = app.init

    modelRef <- Ref.new initial.model

    let runEvent event = do
          model <- Ref.read modelRef

          let Update next = app.update model event

          Ref.write next.model modelRef
          modifyState this $ \state -> state { model = next.model }
          runAffs next.affs

          -- Perform subscriptions
          fold $ app.subs next.model runEvent

        runAffs affs = traverse_ (runAff_ $ either (log <<< show) runEvent) affs

    pure
      { state: { model: initial.model }
      , componentDidMount: runAffs initial.affs
      , render: (\state -> toReactHtml runEvent $ app.view state.model) <$> getState this
      }

runIn :: forall model event. String -> Application model event -> Effect Unit
runIn nodeId app = do
    win  <- DOM.window
    node <- DOM.toNonElementParentNode <$> DOM.document win

    DOM.getElementById nodeId node >>=
        case _ of
        Just root -> render (createElement (classFromApp app) {} []) root *> mempty
        Nothing   -> errorShow $ "Can't find an element with an id " <> nodeId

run :: forall model event. Application model event -> Effect Unit
run = runIn "gimel"
