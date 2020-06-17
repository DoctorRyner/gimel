module Gimel.Engine where

import Prelude

import Data.Array (find, mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, for_, sequence_, traverse_)
import Data.FoldableWithIndex (forWithIndex_, traverseWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (runAff_, Aff)
import Effect.Class.Console (logShow)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Gimel.Html (toReactHtml)
import Gimel.Sub (Sub(..))
import Gimel.Types (Application, UpdateM(..))
import React (Children, ReactClass, ReactElement, createElement, getState, modifyState)
import React (component) as React
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

type SubRef = Map.Map String (Effect Unit)

classFromApp :: forall event model. Application model event -> ReactClass { children :: Children | () }
classFromApp app = React.component "Gimel" constructor
 where
  constructor this = do
    let Update initial = app.init

    modelRef <- Ref.new initial.model
    subsRef  <- Ref.new (mempty :: SubRef)

    let
      runEvent :: event -> Effect Unit
      runEvent event = do
        model <- Ref.read modelRef

        let Update next = app.update model event

        Ref.write next.model modelRef
        modifyState this $ \state -> state {model = next.model}
        runAffs next.affs

        updSubs next.model

      -- Perform subscriptions
      updSubs :: model -> Effect Unit
      updSubs model = do
        subsStore <- Ref.read subsRef

        let
          subs = app.subs model
          complexSubs =
            mapMaybe
              (case _ of
                Sub x -> Just $ Tuple x.id x.attach
                _     -> Nothing
              )
              subs
          simpleSubs =
            mapMaybe
              (case _ of
                SubSimple f -> Just f
                _           -> Nothing
              )
              subs

        -- run simple subscriptions
        foldMap (\f -> f runEvent) simpleSubs

        -- run complex subscriptions
        newSubs <-
          mapMaybe
            identity
            <$>
              traverse
                (\(Tuple id attach) ->
                  case Map.lookup id subsStore of
                    Just detach -> pure $ Just $ Tuple id detach
                    Nothing     -> Just <<< Tuple id <$> attach runEvent
                )
                complexSubs

        -- Detach subscriptions if we can't find id
        let newSubsStore = Map.fromFoldable newSubs

        sequence_ $ Map.difference subsStore newSubsStore

        Ref.write newSubsStore subsRef

      runMaybeEvent :: Maybe event -> Effect Unit
      runMaybeEvent x = maybe mempty runEvent x

      runAffs :: Array (Aff (Maybe event)) -> Effect Unit 
      runAffs affs = traverse_ (runAff_ $ either (log <<< show) runMaybeEvent) affs

      renderHtml :: {model :: model} -> ReactElement
      renderHtml state = toReactHtml runEvent $ app.view state.model

    pure
      { state: {model: initial.model}
      , componentDidMount: do
          updSubs initial.model
          runAffs initial.affs
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
