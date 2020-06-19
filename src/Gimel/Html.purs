module Gimel.Html where

import Prelude

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Attributes (Attribute, toReactProp)
import React (Children, ReactClass, ReactElement, unsafeCreateElement)
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM (text) as DOM
import React.DOM.Props (Props, unsafeFromPropsArray)

data Html event
  = Html ReactEl (Array (Attribute event)) (Array (Html event))
  | Text String
  | Fragment (Array (Html event))
  | RawReact ReactElement

instance semigroupHtml :: Semigroup (Html event) where
  append x y = Fragment [x, y]

instance monoidHtml :: Monoid (Html event) where
  mempty = Fragment []

text :: forall event. String -> Html event
text = Text

textS :: forall event a. Show a => a -> Html event
textS = Text <<< show

reactNodeFromTag :: String -> Array Props -> Array ReactElement -> ReactElement
reactNodeFromTag = mkDOM (IsDynamic false)

el :: forall event. String -> Array (Attribute event) -> Array (Html event) -> Html event
el tagName = Html (reactNodeFromTag tagName)

el_ :: forall event. String -> Array (Attribute event) -> Html event -> Html event
el_ tagName attrs child = Html (reactNodeFromTag tagName) attrs [child]

el' :: forall event. String -> Array (Html event) -> Html event
el' tagName = Html (reactNodeFromTag tagName) []

elAttrs :: String -> ElAttrs
elAttrs tagName attrs = el tagName attrs []

raw :: forall event. ReactElement -> Html event
raw = RawReact

react
  :: forall props event
  .  ReactClass { children :: Children | props }
  -> Array (Attribute event)
  -> Array (Html event)
  -> Html event
react class_ = Html (unsafeCreateElement class_ <<< unsafeFromPropsArray)
      
toReactHtml :: forall event. (event -> Effect Unit) -> Html event -> ReactElement
toReactHtml runEvent = case _ of
  Text str                  -> DOM.text str
  RawReact element          -> element
  Fragment htmls            -> fold $ map (toReactHtml runEvent) htmls
  Html element attrs childs -> element (map (toReactProp runEvent) attrs)
                                       (map (toReactHtml runEvent) childs)

-- Shortcut types
type ReactEl = Array Props -> Array ReactElement -> ReactElement
type El      = forall event. Array (Attribute event) -> Array (Html event) -> Html event
type El_     = forall event. Array (Attribute event) -> Html event -> Html event
type El'     = forall event. Array (Html event) -> Html event
type ElAttrs = forall event. Array (Attribute event) -> Html event

-- Tags

a_ :: forall event. Array (Attribute event) -> Html event -> Html event
a_ = el_ "a"

a :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
a = el "a"

a' :: forall event. Array (Html event) -> Html event
a' = el' "a"

abbr_ :: forall event. Array (Attribute event) -> Html event -> Html event
abbr_ = el_ "abbr"

abbr :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
abbr = el "abbr"

abbr' :: forall event. Array (Html event) -> Html event
abbr' = el' "abbr"

address_ :: forall event. Array (Attribute event) -> Html event -> Html event
address_ = el_ "address"

address :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
address = el "address"

address' :: forall event. Array (Html event) -> Html event
address' = el' "address"

article_ :: forall event. Array (Attribute event) -> Html event -> Html event
article_ = el_ "article"

article :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
article = el "article"

article' :: forall event. Array (Html event) -> Html event
article' = el' "article"

aside_ :: forall event. Array (Attribute event) -> Html event -> Html event
aside_ = el_ "aside"

aside :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
aside = el "aside"

aside' :: forall event. Array (Html event) -> Html event
aside' = el' "aside"

audio_ :: forall event. Array (Attribute event) -> Html event -> Html event
audio_ = el_ "audio"

audio :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
audio = el "audio"

audio' :: forall event. Array (Html event) -> Html event
audio' = el' "audio"

b_ :: forall event. Array (Attribute event) -> Html event -> Html event
b_ = el_ "b"

b :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
b = el "b"

b' :: forall event. Array (Html event) -> Html event
b' = el' "b"

base_ :: forall event. Array (Attribute event) -> Html event -> Html event
base_ = el_ "base"

base :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
base = el "base"

base' :: forall event. Array (Html event) -> Html event
base' = el' "base"

bdi_ :: forall event. Array (Attribute event) -> Html event -> Html event
bdi_ = el_ "bdi"

bdi :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
bdi = el "bdi"

bdi' :: forall event. Array (Html event) -> Html event
bdi' = el' "bdi"

bdo_ :: forall event. Array (Attribute event) -> Html event -> Html event
bdo_ = el_ "bdo"

bdo :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
bdo = el "bdo"

bdo' :: forall event. Array (Html event) -> Html event
bdo' = el' "bdo"

blockquote_ :: forall event. Array (Attribute event) -> Html event -> Html event
blockquote_ = el_ "blockquote"

blockquote :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
blockquote = el "blockquote"

blockquote' :: forall event. Array (Html event) -> Html event
blockquote' = el' "blockquote"

body_ :: forall event. Array (Attribute event) -> Html event -> Html event
body_ = el_ "body"

body :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
body = el "body"

body' :: forall event. Array (Html event) -> Html event
body' = el' "body"

button_ :: forall event. Array (Attribute event) -> Html event -> Html event
button_ = el_ "button"

button :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
button = el "button"

button' :: forall event. Array (Html event) -> Html event
button' = el' "button"

canvas_ :: forall event. Array (Attribute event) -> Html event -> Html event
canvas_ = el_ "canvas"

canvas :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
canvas = el "canvas"

canvas' :: forall event. Array (Html event) -> Html event
canvas' = el' "canvas"

caption_ :: forall event. Array (Attribute event) -> Html event -> Html event
caption_ = el_ "caption"

caption :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
caption = el "caption"

caption' :: forall event. Array (Html event) -> Html event
caption' = el' "caption"

cite_ :: forall event. Array (Attribute event) -> Html event -> Html event
cite_ = el_ "cite"

cite :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
cite = el "cite"

cite' :: forall event. Array (Html event) -> Html event
cite' = el' "cite"

code_ :: forall event. Array (Attribute event) -> Html event -> Html event
code_ = el_ "code"

code :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
code = el "code"

code' :: forall event. Array (Html event) -> Html event
code' = el' "code"

col_ :: forall event. Array (Attribute event) -> Html event -> Html event
col_ = el_ "col"

col :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
col = el "col"

col' :: forall event. Array (Html event) -> Html event
col' = el' "col"

colgroup_ :: forall event. Array (Attribute event) -> Html event -> Html event
colgroup_ = el_ "colgroup"

colgroup :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
colgroup = el "colgroup"

colgroup' :: forall event. Array (Html event) -> Html event
colgroup' = el' "colgroup"

data_ :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
data_ = el "data"

data' :: forall event. Array (Html event) -> Html event
data' = el' "data"

datalist_ :: forall event. Array (Attribute event) -> Html event -> Html event
datalist_ = el_ "datalist"

datalist :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
datalist = el "datalist"

datalist' :: forall event. Array (Html event) -> Html event
datalist' = el' "datalist"

dd_ :: forall event. Array (Attribute event) -> Html event -> Html event
dd_ = el_ "dd"

dd :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
dd = el "dd"

dd' :: forall event. Array (Html event) -> Html event
dd' = el' "dd"

del_ :: forall event. Array (Attribute event) -> Html event -> Html event
del_ = el_ "del"

del :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
del = el "del"

del' :: forall event. Array (Html event) -> Html event
del' = el' "del"

details_ :: forall event. Array (Attribute event) -> Html event -> Html event
details_ = el_ "details"

details :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
details = el "details"

details' :: forall event. Array (Html event) -> Html event
details' = el' "details"

dfn_ :: forall event. Array (Attribute event) -> Html event -> Html event
dfn_ = el_ "dfn"

dfn :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
dfn = el "dfn"

dfn' :: forall event. Array (Html event) -> Html event
dfn' = el' "dfn"

dialog_ :: forall event. Array (Attribute event) -> Html event -> Html event
dialog_ = el_ "dialog"

dialog :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
dialog = el "dialog"

dialog' :: forall event. Array (Html event) -> Html event
dialog' = el' "dialog"

div_ :: forall event. Array (Attribute event) -> Html event -> Html event
div_ = el_ "div"

div :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
div = el "div"

div' :: forall event. Array (Html event) -> Html event
div' = el' "div"

dl_ :: forall event. Array (Attribute event) -> Html event -> Html event
dl_ = el_ "dl"

dl :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
dl = el "dl"

dl' :: forall event. Array (Html event) -> Html event
dl' = el' "dl"

dt_ :: forall event. Array (Attribute event) -> Html event -> Html event
dt_ = el_ "dt"

dt :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
dt = el "dt"

dt' :: forall event. Array (Html event) -> Html event
dt' = el' "dt"

em_ :: forall event. Array (Attribute event) -> Html event -> Html event
em_ = el_ "em"

em :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
em = el "em"

em' :: forall event. Array (Html event) -> Html event
em' = el' "em"

embed_ :: forall event. Array (Attribute event) -> Html event -> Html event
embed_ = el_ "embed"

embed :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
embed = el "embed"

embed' :: forall event. Array (Html event) -> Html event
embed' = el' "embed"

fieldset_ :: forall event. Array (Attribute event) -> Html event -> Html event
fieldset_ = el_ "fieldset"

fieldset :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
fieldset = el "fieldset"

fieldset' :: forall event. Array (Html event) -> Html event
fieldset' = el' "fieldset"

figcaption_ :: forall event. Array (Attribute event) -> Html event -> Html event
figcaption_ = el_ "figcaption"

figcaption :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
figcaption = el "figcaption"

figcaption' :: forall event. Array (Html event) -> Html event
figcaption' = el' "figcaption"

figure_ :: forall event. Array (Attribute event) -> Html event -> Html event
figure_ = el_ "figure"

figure :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
figure = el "figure"

figure' :: forall event. Array (Html event) -> Html event
figure' = el' "figure"

footer_ :: forall event. Array (Attribute event) -> Html event -> Html event
footer_ = el_ "footer"

footer :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
footer = el "footer"

footer' :: forall event. Array (Html event) -> Html event
footer' = el' "footer"

form_ :: forall event. Array (Attribute event) -> Html event -> Html event
form_ = el_ "form"

form :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
form = el "form"

form' :: forall event. Array (Html event) -> Html event
form' = el' "form"

h1_ :: forall event. Array (Attribute event) -> Html event -> Html event
h1_ = el_ "h1"

h1 :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
h1 = el "h1"

h1' :: forall event. Array (Html event) -> Html event
h1' = el' "h1"

h2_ :: forall event. Array (Attribute event) -> Html event -> Html event
h2_ = el_ "h2"

h2 :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
h2 = el "h2"

h2' :: forall event. Array (Html event) -> Html event
h2' = el' "h2"

h3_ :: forall event. Array (Attribute event) -> Html event -> Html event
h3_ = el_ "h3"

h3 :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
h3 = el "h3"

h3' :: forall event. Array (Html event) -> Html event
h3' = el' "h3"

h4_ :: forall event. Array (Attribute event) -> Html event -> Html event
h4_ = el_ "h4"

h4 :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
h4 = el "h4"

h4' :: forall event. Array (Html event) -> Html event
h4' = el' "h4"

h5_ :: forall event. Array (Attribute event) -> Html event -> Html event
h5_ = el_ "h5"

h5 :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
h5 = el "h5"

h5' :: forall event. Array (Html event) -> Html event
h5' = el' "h5"

h6_ :: forall event. Array (Attribute event) -> Html event -> Html event
h6_ = el_ "h6"

h6 :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
h6 = el "h6"

h6' :: forall event. Array (Html event) -> Html event
h6' = el' "h6"

head_ :: forall event. Array (Attribute event) -> Html event -> Html event
head_ = el_ "head"

head :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
head = el "head"

head' :: forall event. Array (Html event) -> Html event
head' = el' "head"

header_ :: forall event. Array (Attribute event) -> Html event -> Html event
header_ = el_ "header"

header :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
header = el "header"

header' :: forall event. Array (Html event) -> Html event
header' = el' "header"

hgroup_ :: forall event. Array (Attribute event) -> Html event -> Html event
hgroup_ = el_ "hgroup"

hgroup :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
hgroup = el "hgroup"

hgroup' :: forall event. Array (Html event) -> Html event
hgroup' = el' "hgroup"

hr_ :: forall event. Array (Attribute event) -> Html event -> Html event
hr_ = el_ "hr"

hr :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
hr = el "hr"

hr' :: forall event. Array (Html event) -> Html event
hr' = el' "hr"

html_ :: forall event. Array (Attribute event) -> Html event -> Html event
html_ = el_ "html"

html :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
html = el "html"

html' :: forall event. Array (Html event) -> Html event
html' = el' "html"

i_ :: forall event. Array (Attribute event) -> Html event -> Html event
i_ = el_ "i"

i :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
i = el "i"

i' :: forall event. Array (Html event) -> Html event
i' = el' "i"

iframe_ :: forall event. Array (Attribute event) -> Html event -> Html event
iframe_ = el_ "iframe"

iframe :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
iframe = el "iframe"

iframe' :: forall event. Array (Html event) -> Html event
iframe' = el' "iframe"

ins_ :: forall event. Array (Attribute event) -> Html event -> Html event
ins_ = el_ "ins"

ins :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
ins = el "ins"

ins' :: forall event. Array (Html event) -> Html event
ins' = el' "ins"

kbd_ :: forall event. Array (Attribute event) -> Html event -> Html event
kbd_ = el_ "kbd"

kbd :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
kbd = el "kbd"

kbd' :: forall event. Array (Html event) -> Html event
kbd' = el' "kbd"

label_ :: forall event. Array (Attribute event) -> Html event -> Html event
label_ = el_ "label"

label :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
label = el "label"

label' :: forall event. Array (Html event) -> Html event
label' = el' "label"

legend_ :: forall event. Array (Attribute event) -> Html event -> Html event
legend_ = el_ "legend"

legend :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
legend = el "legend"

legend' :: forall event. Array (Html event) -> Html event
legend' = el' "legend"

li_ :: forall event. Array (Attribute event) -> Html event -> Html event
li_ = el_ "li"

li :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
li = el "li"

li' :: forall event. Array (Html event) -> Html event
li' = el' "li"

link_ :: forall event. Array (Attribute event) -> Html event -> Html event
link_ = el_ "link"

link :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
link = el "link"

link' :: forall event. Array (Html event) -> Html event
link' = el' "link"

main_ :: forall event. Array (Attribute event) -> Html event -> Html event
main_ = el_ "main"

main :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
main = el "main"

main' :: forall event. Array (Html event) -> Html event
main' = el' "main"

map_ :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
map_ = el "map"

map' :: forall event. Array (Html event) -> Html event
map' = el' "map"

mark_ :: forall event. Array (Attribute event) -> Html event -> Html event
mark_ = el_ "mark"

mark :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
mark = el "mark"

mark' :: forall event. Array (Html event) -> Html event
mark' = el' "mark"

math_ :: forall event. Array (Attribute event) -> Html event -> Html event
math_ = el_ "math"

math :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
math = el "math"

math' :: forall event. Array (Html event) -> Html event
math' = el' "math"

menu_ :: forall event. Array (Attribute event) -> Html event -> Html event
menu_ = el_ "menu"

menu :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
menu = el "menu"

menu' :: forall event. Array (Html event) -> Html event
menu' = el' "menu"

menuitem_ :: forall event. Array (Attribute event) -> Html event -> Html event
menuitem_ = el_ "menuitem"

menuitem :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
menuitem = el "menuitem"

menuitem' :: forall event. Array (Html event) -> Html event
menuitem' = el' "menuitem"

meta_ :: forall event. Array (Attribute event) -> Html event -> Html event
meta_ = el_ "meta"

meta :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
meta = el "meta"

meta' :: forall event. Array (Html event) -> Html event
meta' = el' "meta"

meter_ :: forall event. Array (Attribute event) -> Html event -> Html event
meter_ = el_ "meter"

meter :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
meter = el "meter"

meter' :: forall event. Array (Html event) -> Html event
meter' = el' "meter"

nav_ :: forall event. Array (Attribute event) -> Html event -> Html event
nav_ = el_ "nav"

nav :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
nav = el "nav"

nav' :: forall event. Array (Html event) -> Html event
nav' = el' "nav"

noscript_ :: forall event. Array (Attribute event) -> Html event -> Html event
noscript_ = el_ "noscript"

noscript :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
noscript = el "noscript"

noscript' :: forall event. Array (Html event) -> Html event
noscript' = el' "noscript"

object_ :: forall event. Array (Attribute event) -> Html event -> Html event
object_ = el_ "object"

object :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
object = el "object"

object' :: forall event. Array (Html event) -> Html event
object' = el' "object"

ol_ :: forall event. Array (Attribute event) -> Html event -> Html event
ol_ = el_ "ol"

ol :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
ol = el "ol"

ol' :: forall event. Array (Html event) -> Html event
ol' = el' "ol"

optgroup_ :: forall event. Array (Attribute event) -> Html event -> Html event
optgroup_ = el_ "optgroup"

optgroup :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
optgroup = el "optgroup"

optgroup' :: forall event. Array (Html event) -> Html event
optgroup' = el' "optgroup"

option_ :: forall event. Array (Attribute event) -> Html event -> Html event
option_ = el_ "option"

option :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
option = el "option"

option' :: forall event. Array (Html event) -> Html event
option' = el' "option"

output_ :: forall event. Array (Attribute event) -> Html event -> Html event
output_ = el_ "output"

output :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
output = el "output"

output' :: forall event. Array (Html event) -> Html event
output' = el' "output"

p_ :: forall event. Array (Attribute event) -> Html event -> Html event
p_ = el_ "p"

p :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
p = el "p"

p' :: forall event. Array (Html event) -> Html event
p' = el' "p"

param_ :: forall event. Array (Attribute event) -> Html event -> Html event
param_ = el_ "param"

param :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
param = el "param"

param' :: forall event. Array (Html event) -> Html event
param' = el' "param"

picture_ :: forall event. Array (Attribute event) -> Html event -> Html event
picture_ = el_ "picture"

picture :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
picture = el "picture"

picture' :: forall event. Array (Html event) -> Html event
picture' = el' "picture"

pre_ :: forall event. Array (Attribute event) -> Html event -> Html event
pre_ = el_ "pre"

pre :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
pre = el "pre"

pre' :: forall event. Array (Html event) -> Html event
pre' = el' "pre"

progress_ :: forall event. Array (Attribute event) -> Html event -> Html event
progress_ = el_ "progress"

progress :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
progress = el "progress"

progress' :: forall event. Array (Html event) -> Html event
progress' = el' "progress"

q_ :: forall event. Array (Attribute event) -> Html event -> Html event
q_ = el_ "q"

q :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
q = el "q"

q' :: forall event. Array (Html event) -> Html event
q' = el' "q"

rb_ :: forall event. Array (Attribute event) -> Html event -> Html event
rb_ = el_ "rb"

rb :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
rb = el "rb"

rb' :: forall event. Array (Html event) -> Html event
rb' = el' "rb"

rp_ :: forall event. Array (Attribute event) -> Html event -> Html event
rp_ = el_ "rp"

rp :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
rp = el "rp"

rp' :: forall event. Array (Html event) -> Html event
rp' = el' "rp"

rt_ :: forall event. Array (Attribute event) -> Html event -> Html event
rt_ = el_ "rt"

rt :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
rt = el "rt"

rt' :: forall event. Array (Html event) -> Html event
rt' = el' "rt"

rtc_ :: forall event. Array (Attribute event) -> Html event -> Html event
rtc_ = el_ "rtc"

rtc :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
rtc = el "rtc"

rtc' :: forall event. Array (Html event) -> Html event
rtc' = el' "rtc"

ruby_ :: forall event. Array (Attribute event) -> Html event -> Html event
ruby_ = el_ "ruby"

ruby :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
ruby = el "ruby"

ruby' :: forall event. Array (Html event) -> Html event
ruby' = el' "ruby"

s_ :: forall event. Array (Attribute event) -> Html event -> Html event
s_ = el_ "s"

s :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
s = el "s"

s' :: forall event. Array (Html event) -> Html event
s' = el' "s"

samp_ :: forall event. Array (Attribute event) -> Html event -> Html event
samp_ = el_ "samp"

samp :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
samp = el "samp"

samp' :: forall event. Array (Html event) -> Html event
samp' = el' "samp"

script_ :: forall event. Array (Attribute event) -> Html event -> Html event
script_ = el_ "script"

script :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
script = el "script"

script' :: forall event. Array (Html event) -> Html event
script' = el' "script"

section_ :: forall event. Array (Attribute event) -> Html event -> Html event
section_ = el_ "section"

section :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
section = el "section"

section' :: forall event. Array (Html event) -> Html event
section' = el' "section"

select_ :: forall event. Array (Attribute event) -> Html event -> Html event
select_ = el_ "select"

select :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
select = el "select"

select' :: forall event. Array (Html event) -> Html event
select' = el' "select"

slot_ :: forall event. Array (Attribute event) -> Html event -> Html event
slot_ = el_ "slot"

slot :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
slot = el "slot"

slot' :: forall event. Array (Html event) -> Html event
slot' = el' "slot"

small_ :: forall event. Array (Attribute event) -> Html event -> Html event
small_ = el_ "small"

small :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
small = el "small"

small' :: forall event. Array (Html event) -> Html event
small' = el' "small"

source_ :: forall event. Array (Attribute event) -> Html event -> Html event
source_ = el_ "source"

source :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
source = el "source"

source' :: forall event. Array (Html event) -> Html event
source' = el' "source"

span_ :: forall event. Array (Attribute event) -> Html event -> Html event
span_ = el_ "span"

span :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
span = el "span"

span' :: forall event. Array (Html event) -> Html event
span' = el' "span"

strong_ :: forall event. Array (Attribute event) -> Html event -> Html event
strong_ = el_ "strong"

strong :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
strong = el "strong"

strong' :: forall event. Array (Html event) -> Html event
strong' = el' "strong"

style_ :: forall event. Array (Attribute event) -> Html event -> Html event
style_ = el_ "style"

style :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
style = el "style"

style' :: forall event. Array (Html event) -> Html event
style' = el' "style"

sub_ :: forall event. Array (Attribute event) -> Html event -> Html event
sub_ = el_ "sub"

sub :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
sub = el "sub"

sub' :: forall event. Array (Html event) -> Html event
sub' = el' "sub"

summary_ :: forall event. Array (Attribute event) -> Html event -> Html event
summary_ = el_ "summary"

summary :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
summary = el "summary"

summary' :: forall event. Array (Html event) -> Html event
summary' = el' "summary"

sup_ :: forall event. Array (Attribute event) -> Html event -> Html event
sup_ = el_ "sup"

sup :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
sup = el "sup"

sup' :: forall event. Array (Html event) -> Html event
sup' = el' "sup"

svg_ :: forall event. Array (Attribute event) -> Html event -> Html event
svg_ = el_ "svg"

svg :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
svg = el "svg"

svg' :: forall event. Array (Html event) -> Html event
svg' = el' "svg"

table_ :: forall event. Array (Attribute event) -> Html event -> Html event
table_ = el_ "table"

table :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
table = el "table"

table' :: forall event. Array (Html event) -> Html event
table' = el' "table"

tbody_ :: forall event. Array (Attribute event) -> Html event -> Html event
tbody_ = el_ "tbody"

tbody :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
tbody = el "tbody"

tbody' :: forall event. Array (Html event) -> Html event
tbody' = el' "tbody"

td_ :: forall event. Array (Attribute event) -> Html event -> Html event
td_ = el_ "td"

td :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
td = el "td"

td' :: forall event. Array (Html event) -> Html event
td' = el' "td"

template_ :: forall event. Array (Attribute event) -> Html event -> Html event
template_ = el_ "template"

template :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
template = el "template"

template' :: forall event. Array (Html event) -> Html event
template' = el' "template"

textarea_ :: forall event. Array (Attribute event) -> Html event -> Html event
textarea_ = el_ "textarea"

textarea :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
textarea = el "textarea"

textarea' :: forall event. Array (Html event) -> Html event
textarea' = el' "textarea"

tfoot_ :: forall event. Array (Attribute event) -> Html event -> Html event
tfoot_ = el_ "tfoot"

tfoot :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
tfoot = el "tfoot"

tfoot' :: forall event. Array (Html event) -> Html event
tfoot' = el' "tfoot"

th_ :: forall event. Array (Attribute event) -> Html event -> Html event
th_ = el_ "th"

th :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
th = el "th"

th' :: forall event. Array (Html event) -> Html event
th' = el' "th"

thead_ :: forall event. Array (Attribute event) -> Html event -> Html event
thead_ = el_ "thead"

thead :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
thead = el "thead"

thead' :: forall event. Array (Html event) -> Html event
thead' = el' "thead"

time_ :: forall event. Array (Attribute event) -> Html event -> Html event
time_ = el_ "time"

time :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
time = el "time"

time' :: forall event. Array (Html event) -> Html event
time' = el' "time"

title_ :: forall event. Array (Attribute event) -> Html event -> Html event
title_ = el_ "title"

title :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
title = el "title"

title' :: forall event. Array (Html event) -> Html event
title' = el' "title"

tr_ :: forall event. Array (Attribute event) -> Html event -> Html event
tr_ = el_ "tr"

tr :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
tr = el "tr"

tr' :: forall event. Array (Html event) -> Html event
tr' = el' "tr"

track_ :: forall event. Array (Attribute event) -> Html event -> Html event
track_ = el_ "track"

track :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
track = el "track"

track' :: forall event. Array (Html event) -> Html event
track' = el' "track"

u_ :: forall event. Array (Attribute event) -> Html event -> Html event
u_ = el_ "u"

u :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
u = el "u"

u' :: forall event. Array (Html event) -> Html event
u' = el' "u"

ul_ :: forall event. Array (Attribute event) -> Html event -> Html event
ul_ = el_ "ul"

ul :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
ul = el "ul"

ul' :: forall event. Array (Html event) -> Html event
ul' = el' "ul"

var_ :: forall event. Array (Attribute event) -> Html event -> Html event
var_ = el_ "var"

var :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
var = el "var"

var' :: forall event. Array (Html event) -> Html event
var' = el' "var"

video_ :: forall event. Array (Attribute event) -> Html event -> Html event
video_ = el_ "video"

video :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
video = el "video"

video' :: forall event. Array (Html event) -> Html event
video' = el' "video"

wbr_ :: forall event. Array (Attribute event) -> Html event -> Html event
wbr_ = el_ "wbr"

wbr :: forall event. Array (Attribute event) -> Array (Html event) -> Html event
wbr = el "wbr"

wbr' :: forall event. Array (Html event) -> Html event
wbr' = el' "wbr"

img :: ElAttrs
img = elAttrs "img"

input :: ElAttrs
input = elAttrs "input"

br :: forall event. Html event
br = el "br" [] []

area :: ElAttrs
area = elAttrs "area"
