module Gimel.Engine where

import Prelude

import Data.Array (find)
import Data.Either (either)
import Data.Foldable (foldMap, traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (runAff_, Aff)
import Effect.Console (errorShow, log)
import Effect.Ref as Ref
import Gimel.Html (toReactHtml)
import Gimel.Sub (Sub(..))
import Gimel.Types (Application, UpdateM(..))
import Gimel.Utils (justs)
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

        let subs = app.subs model

        let
          runSub :: Sub event -> Effect Unit
          runSub = case _ of
            SubSimple f -> f runEvent
            Sub       s -> do
              let mbDetach = Map.lookup s.id subsStore
              case mbDetach of
                Just x  -> pure unit
                Nothing -> do
                  detach <- s.attach runEvent
                  Ref.write (Map.insert s.id detach subsStore) subsRef
              mempty

        -- run subscriptions
        foldMap runSub subs

        -- Detach subscriptions if we can't find id
        let
          subsWithId =
            justs $
              map
                (case _ of
                  Sub x -> Just x
                  _     -> Nothing
                )
                subs

        traverseWithIndex_
          (\s detach -> do
            case find (\a -> s == a.id) subsWithId of
              Just _  -> mempty
              Nothing -> do
                detach
                Ref.write (Map.delete s subsStore) subsRef
          )
          subsStore

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
