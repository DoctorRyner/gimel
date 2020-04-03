module Gimel.Engine where

import Prelude

import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Gimel.Html (Html(..), toReactHtml)
import Gimel.Types (Application)
import React (Children, ReactClass, createElement, getState, modifyState)
import React (component) as React
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

infix 4 Tuple as <:

component :: forall model event. Application model event -> Html event
component app = RawReact $ createElement (mkGimelApp app) {} []

mkGimelApp :: forall event model. Application model event -> ReactClass { children :: Children | () }
mkGimelApp app = React.component "Gimel" constructor
 where
  constructor this = do
    let Tuple initialModel initialAffs = app.init

    modelRef <- Ref.new initialModel

    let runEvent event = do
          model <- Ref.read modelRef

          let Tuple nextModel nextAffs = app.update model event

          Ref.write nextModel modelRef

          modifyState this $ \state -> state { model = nextModel }

          runAffs nextAffs

        runAffs affs = traverse_ (runAff_ $ either (log <<< show) runEvent) affs

    pure
      { state: { model: initialModel }
      , componentDidMount: runAffs initialAffs
      , render: (\state -> toReactHtml runEvent $ app.view state.model) <$> getState this
      }

runIn :: forall model event. String -> Application model event -> Effect Unit
runIn nodeId app = do
  win <- DOM.window
  doc <- DOM.document win

  let node = DOM.toNonElementParentNode doc

  DOM.getElementById nodeId node >>=
    case _ of
      Just root -> render (createElement (mkGimelApp app) {} []) root *> mempty
      Nothing   -> errorShow $ "Can't find an element with an id " <> nodeId

run :: forall model event. Application model event -> Effect Unit
run = runIn "gimel"
