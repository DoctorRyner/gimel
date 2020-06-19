module Gimel.Html.Styled where

import Prelude

import CSS (CSS, fromString, prefixed, render, renderedInline)
import Data.Maybe (fromMaybe)
import Gimel.Attributes (Attribute)
import Gimel.Html (Html, react)
import React (ReactClass)

foreign import styledRaw :: forall a. String -> String -> ReactClass a

custom :: String -> String -> CSS
custom = prefixed <<< fromString

infix 4 custom as |:

renderCSS :: CSS -> String
renderCSS = fromMaybe "<!INVALID-CSS!>" <<< renderedInline <<< render

styled :: forall event. String -> CSS -> Array (Attribute event) -> Array (Html event) -> Html event
styled tag css = react (styledRaw tag $ renderCSS css)

type Styled event = CSS -> Array (Attribute event) -> Array (Html event) -> Html event
