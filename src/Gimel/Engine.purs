module Gimel.Engine where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (runAff_, Aff)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Gimel.Html (toReactHtml)
import Gimel.Sub (Sub(..), WhenSub, SubRef)
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
      onceSubs :: Array (model -> (event -> Effect Unit) -> Effect Unit)
      onceSubs =
        mapMaybe
          (case _ of
            Once f -> Just f
            _      -> Nothing
          )
          app.subs

      alwaysSubs :: Array (model -> (event -> Effect Unit) -> Effect Unit)
      alwaysSubs =
        mapMaybe
          (case _ of
            Always f -> Just f
            _        -> Nothing
          )
          app.subs

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

      componentDidMount :: Effect Unit
      componentDidMount = do
        runAffs initial.affs
        runOnceSubs onceSubs

      initialWhenSubs :: Array (WhenSub model event)
      initialWhenSubs =
        mapMaybe
          (case _ of
            When x -> Just x
            _      -> Nothing
          )
          app.subs

    subs <-
      traverse
        (\sub ->
            if sub.condition initial.model
            then do
              detach <- sub.attach initial.model runEvent
              pure $ Right {condition: sub.condition, attach: sub.attach, detach}
            else pure $ Left {condition: sub.condition, attach: sub.attach}
        )
        initialWhenSubs

    subsRef <- Ref.new (subs :: SubRef model event)

    pure
      { state: {model: initial.model}
      , componentDidMount
      , render: do
          state <- getState this

          -- Perform Always subs
          traverse_ (\f -> f state.model runEvent) alwaysSubs

          -- Perform When subs
          whenSubs <- Ref.read subsRef

          newSubs <-
            traverse
              (\sub -> do
                  currState <- getState this
                  let
                    condition =
                      case sub of
                        Right x -> x.condition currState.model
                        Left x  -> x.condition currState.model

                  if condition
                  then
                    case sub of
                      Right _ -> pure sub
                      Left  s -> do
                        detach <- s.attach currState.model runEvent
                        pure $
                          Right
                            { condition:
                                case sub of
                                  Right x -> x.condition
                                  Left  x -> x.condition
                            , attach: s.attach
                            , detach
                            }
                  else
                    case sub of
                      Left _  -> pure sub
                      Right s -> do
                        s.detach
                        pure $
                          Left
                            { condition:
                                case sub of
                                  Right x -> x.condition
                                  Left  x -> x.condition
                            , attach: s.attach
                            }
              )
              whenSubs

          Ref.write newSubs subsRef

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
