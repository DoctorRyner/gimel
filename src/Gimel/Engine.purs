module Gimel.Engine where

import Prelude

import Data.Array (cons, uncons)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Ref as Ref
import Gimel.Cmd (Cmd(..))
import Gimel.Html (Html, toReactHtml)
import Gimel.Sub (Sub(..), SubInstance, SubStatus(..), none)
import Gimel.Types (Application, Update, UpdateM(..))
import React (Children, ReactClass, createElement, getState, modifyState)
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
    modelRef <- Ref.new app.init

    let
      runEvent :: event -> Aff Unit
      runEvent event = do
        model <- liftEffect $ Ref.read modelRef

        let Update next = app.update model event

        liftEffect do
          Ref.write next.model modelRef
          modifyState this $ \state -> state {model = next.model}

        runCmds next.cmds

      runCmds          = traverse_ (\(Cmd x) -> x runEvent)
      renderHtml state = toReactHtml runEvent $ app.view state.model

      collectSubs
        :: Array (Sub model event)
        -> {always :: Array (model -> Cmd event), active :: Array (SubInstance model event)}
        -> {always :: Array (model -> Cmd event), active :: Array (SubInstance model event)}
      collectSubs subs res = case uncons subs of
        Nothing                   -> res
        Just {head: x , tail: xs} ->
          case x of
            Always f        -> collectSubs xs $ res {always = cons f res.always}
            Sub sub         -> collectSubs xs $ res {active = cons sub res.active}
            Batch innerSubs -> collectSubs xs $ res <> collectSubs innerSubs mempty

      collectedSubs = collectSubs [app.subs] mempty

      updateActiveSub :: SubInstance model event -> Aff (SubInstance model event)
      updateActiveSub sub = do
        currState <- liftEffect $ getState this
        status    <-
          case sub.status of
            Active {disable} ->
              if sub.checkCondition currState.model
              then pure sub.status
              else disable $> Inactive
            Inactive ->
              if sub.checkCondition currState.model
              then do
                disable <- sub.enable currState.model runEvent
                pure $ Active {disable}
              else pure Inactive

        pure sub {status = status}

      initActiveSubs :: Aff (Array (SubInstance model event))
      initActiveSubs =
        traverse
          (\sub ->
              if sub.checkCondition app.init
              then do
                disable <- sub.enable app.init runEvent
                pure sub {status = Active {disable}}
              else pure sub
          )
          collectedSubs.active

    activeSubsRef <- Ref.new []

    pure
      { state: {model: app.init}

      , render: renderHtml <$> getState this

      , componentDidMount: launchAff_ do
          subs <- initActiveSubs
          liftEffect $ Ref.write subs activeSubsRef

      , componentDidUpdate: \_ state _ -> launchAff_ do
          -- Perform Always subs
          runCmds $ map (\f -> f state.model) collectedSubs.always

          -- Update Active subs
          currActiveSubs    <- liftEffect $ Ref.read activeSubsRef
          updatedActiveSubs <- traverse updateActiveSub currActiveSubs

          liftEffect $ Ref.write updatedActiveSubs activeSubsRef
      }

run :: forall model event. Application model event -> Effect Unit
run = runOn "gimel"

runOn :: forall model event. String -> Application model event -> Effect Unit
runOn nodeId app = do
  win       <- DOM.window
  maybeRoot <- DOM.getElementById nodeId =<< DOM.toNonElementParentNode <$> DOM.document win

  case maybeRoot of
    Just root -> render (createElement (classFromApp app) {} []) root *> mempty
    Nothing   -> error $ "Can't find an element with an id " <> nodeId

pureApp
  :: forall model event
  .  {init :: model, view :: model -> Html event, update :: model -> event -> model}
  -> Application model event
pureApp app =
  { init: app.init
  , update: \model -> pure <<< app.update model
  , view: app.view
  , subs: none
  }

sandbox
  :: forall model event
  .  { init :: model
     , view :: model -> Html event
     , update :: model -> event -> Update model event
     }
  -> Application model event
sandbox app =
  { init: app.init
  , update: app.update
  , view: app.view
  , subs: none
  }