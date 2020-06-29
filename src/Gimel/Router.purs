module Gimel.Router where

import CSS (CSS)
import Gimel.Attributes (Attribute, (=:))
import Gimel.Html (Html, react)
import Gimel.Utils (renderCSS)
import React (ReactClass)

foreign import classRouter :: forall a. ReactClass a
foreign import classBrowserRouter :: forall a. ReactClass a
foreign import classStaticRouter :: forall a. ReactClass a
foreign import classRoute :: forall a. ReactClass a
foreign import classSwitch :: forall a. ReactClass a
foreign import classRedirect :: forall a. ReactClass a
foreign import classLink :: forall a. ReactClass a
foreign import classNavLink :: forall a. ReactClass a

router :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
router = react classRouter

browserRouter :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
browserRouter = react classBrowserRouter

basename :: forall event. String -> Attribute event
basename = (=:) "basename"

staticRouter :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
staticRouter = react classStaticRouter

route :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
route = react classRoute

exact :: forall event. Boolean -> Attribute event
exact = (=:) "exact"

path :: forall event. String -> Attribute event
path = (=:) "path"

switch :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
switch = react classSwitch

redirect :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
redirect = react classRedirect

from :: forall event. String -> Attribute event
from = (=:) "from"

to :: forall event. String -> Attribute event
to = (=:) "to"

link :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
link = react classLink

navLink :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
navLink = react classNavLink

activeClassName :: forall event. String -> Attribute event
activeClassName = (=:) "activeClassName"

activeStyle :: forall event. CSS -> Attribute event
activeStyle css = "activeStyle" =: renderCSS css