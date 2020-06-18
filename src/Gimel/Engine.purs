module Gimel.Engine where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (runAff_, Aff)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Gimel.Html (toReactHtml)
import Gimel.Sub (ActiveSubInstance, ActiveSubStatus(..), Sub(..))
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

      runMaybeEvent :: Maybe event -> Effect Unit
      runMaybeEvent x = maybe mempty runEvent x

      runAffs :: Array (Aff (Maybe event)) -> Effect Unit 
      runAffs affs = traverse_ (runAff_ $ either (log <<< show) runMaybeEvent) affs

      renderHtml :: {model :: model} -> ReactElement
      renderHtml state = toReactHtml runEvent $ app.view state.model

      runOnceSubs :: Array (model -> (event -> Effect Unit) -> Effect Unit) -> Effect Unit
      runOnceSubs =
        traverse_
          (\f -> do
            state <- getState this
            f state.model runEvent
          )

      onceSubs =
        mapMaybe
          (case _ of
            Once f -> Just f
            _      -> Nothing
          )
          app.subs

      alwaysSubs =
        mapMaybe
          (case _ of
            Always f -> Just f
            _        -> Nothing
          )
          app.subs

      activeSubs =
        mapMaybe
          (case _ of
            ActiveWhen x -> Just x
            _            -> Nothing
          )
          app.subs

      updateActiveSub :: ActiveSubInstance model event -> Effect (ActiveSubInstance model event)
      updateActiveSub sub = do
        currState <- getState this
        status    <-
          case sub.status of
            Active {stop} ->
              if sub.check currState.model
              then pure $ Active {stop}
              else stop $> Inactive
            Inactive ->
              if sub.check currState.model
              then do
                stop <- sub.activate currState.model runEvent
                pure $ Active {stop}
              else pure Inactive

        pure sub {status = status}

      initActiveSubs :: Effect (Array (ActiveSubInstance model event))
      initActiveSubs =
        traverse
          (\sub ->
              if sub.check initial.model
              then do
                stop <- sub.activate initial.model runEvent
                pure sub {status = Active {stop}}
              else pure sub
          )
          activeSubs

    activeSubsRef <- Ref.new =<< initActiveSubs

    pure
      { state: {model: initial.model}
      , componentDidMount: do
          runAffs initial.affs
          runOnceSubs onceSubs
      , render: do
          state <- getState this

          -- Perform Always subs
          traverse_ (\f -> f state.model runEvent) alwaysSubs

          -- Perform Active subs
          currActiveSubs    <- Ref.read activeSubsRef
          updatedActiveSubs <- traverse updateActiveSub currActiveSubs

          Ref.write updatedActiveSubs activeSubsRef

          pure $ renderHtml state
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
