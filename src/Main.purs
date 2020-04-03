module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Gimel.Attributes (on, onChange, onClick, style, (=:))
import Gimel.Engine (run, (<:))
import Gimel.Html (El, Html, br, react, text)
import Gimel.Types (Update)
import MaterialUI.Button (classButton)
import MaterialUI.Dialog (classDialog)
import MaterialUI.DialogActions (classDialogActions)
import MaterialUI.DialogTitle (classDialogTitle)
import MaterialUI.TextField (classTextField)
import MaterialUI.Typography (classTypography)

data Event
  = Inc
  | Dec
  | IncAuto
  | Batch (Array Event)
  | HandleInput String
  | OpenDialog Boolean
  | SetCounter Int

type Model =
  { counter :: Int
  , inputValue :: String
  , isDialogOpen :: Boolean
  }

initialModel :: Model
initialModel =
  { counter: 0
  , inputValue: ""
  , isDialogOpen: false
  }

typography :: El
typography = react classTypography

btn :: El
btn = react classButton

textField :: El
textField = react classTextField

dialog :: El
dialog = react classDialog

dialogTitle :: El
dialogTitle = react classDialogTitle

dialogActions :: El
dialogActions = react classDialogActions

view :: Model -> Html Event
view model = fold
  [ typography ["variant" =: "h6", "gutterBottom" =: true] [text $ show model]
  , btn [onClick Inc, "color" =: "primary", "variant" =: "contained", style { marginRight: "7px" }] [text "+"]
  , btn [onClick Dec, "color" =: "secondary", "variant" =: "contained"] [text "-"]
  , br, br
  , textField [onChange HandleInput, "variant" =: "outlined", "label" =: "Test input"] []
  , br, br
  , btn [onClick $ OpenDialog true, "color" =: "primary", "variant" =: "contained"] [text "Open dialog"]
  , dialog [on "Close" $ OpenDialog false, "open" =: model.isDialogOpen]
    [ dialogTitle [] [text "Set counter to 0?"]
    , dialogActions []
      [ btn [onClick $ OpenDialog false, "variant" =: "contained", "color" =: "secondary"] [text "No"]
      , btn [onClick $ Batch [OpenDialog false, SetCounter 0], "variant" =: "contained", "color" =: "primary"] [text "Yes"]
      ]
    ]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  Inc -> model { counter = model.counter + 1 } <: []
  Dec -> model { counter = model.counter - 1 } <: []
  IncAuto -> model <: [delay (Milliseconds 1000.0) $> Batch [Inc, IncAuto]]
  HandleInput str -> model { inputValue = str } <: []
  OpenDialog x -> model { isDialogOpen = x } <: []
  SetCounter x -> model { counter = x } <: []
  Batch events -> model <: map pure events

main :: Effect Unit
main = run
  { init: initialModel <: [pure IncAuto]
  , view
  , update
  }

