module Gimel.Html where

import Prelude

import Data.Foldable (fold)
import Gimel.Attributes (Attribute, toReactProp)
import Gimel.Dispatcher (Dispatcher)
import React (Children, ReactClass, ReactElement, unsafeCreateElement)
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM (text) as DOM
import React.DOM.Props (Props, unsafeFromPropsArray)

data Html event
  = Html String (Array (Attribute event)) (Array (Html event))
  | HtmlReact (Array Props -> Array ReactElement -> ReactElement) (Array (Attribute event)) (Array (Html event))
  | Text String
  | Fragment (Array (Html event))
  | RawReact ReactElement

instance semigroupHtml :: Semigroup (Html event) where
  append x y = Fragment [x, y]

instance foldHtml :: Monoid (Html event) where
  mempty = text ""

text :: forall event. String -> Html event
text = Text

type El  = forall event. Array (Attribute event) -> Array (Html event) -> Html event
type El_ = forall event. Array (Attribute event) -> Html event -> Html event
type El' = forall event. Array (Html event) -> Html event
type ElAttrs = forall event. Array (Attribute event) -> Html event

el :: String -> El
el = Html

el_ :: String -> El_
el_ tagName attrs child = Html tagName attrs [child]

el' :: String -> El'
el' tagName = Html tagName []

elAttrs :: String -> ElAttrs
elAttrs tagName attrs = el tagName attrs []

raw :: forall event. ReactElement -> Html event
raw = RawReact

react :: forall props event
      .  ReactClass { children :: Children | props }
      -> Array (Attribute event)
      -> Array (Html event)
      -> Html event
react class_ = HtmlReact (unsafeCreateElement class_ <<< unsafeFromPropsArray)

mkR :: String -> Array Props -> Array ReactElement -> ReactElement
mkR = mkDOM (IsDynamic false)

toReactHtml :: forall event. Dispatcher event -> Html event -> ReactElement
toReactHtml dispatch = case _ of
  Html tagName attrs childs      -> mkR tagName (map (toReactProp dispatch) attrs) (map (toReactHtml dispatch) childs)
  HtmlReact element attrs childs -> element (map (toReactProp dispatch) attrs) (map (toReactHtml dispatch) childs)
  Text str                       -> DOM.text str
  RawReact element               -> element
  Fragment htmls                 -> fold $ map (toReactHtml dispatch) htmls

-- Tags

a_ :: El_
a_ = el_ "a"

a :: El
a = el "a"

a' :: El'
a' = el' "a"

abbr_ :: El_
abbr_ = el_ "abbr"

abbr :: El
abbr = el "abbr"

abbr' :: El'
abbr' = el' "abbr"

address_ :: El_
address_ = el_ "address"

address :: El
address = el "address"

address' :: El'
address' = el' "address"

article_ :: El_
article_ = el_ "article"

article :: El
article = el "article"

article' :: El'
article' = el' "article"

aside_ :: El_
aside_ = el_ "aside"

aside :: El
aside = el "aside"

aside' :: El'
aside' = el' "aside"

audio_ :: El_
audio_ = el_ "audio"

audio :: El
audio = el "audio"

audio' :: El'
audio' = el' "audio"

b_ :: El_
b_ = el_ "b"

b :: El
b = el "b"

b' :: El'
b' = el' "b"

base_ :: El_
base_ = el_ "base"

base :: El
base = el "base"

base' :: El'
base' = el' "base"

bdi_ :: El_
bdi_ = el_ "bdi"

bdi :: El
bdi = el "bdi"

bdi' :: El'
bdi' = el' "bdi"

bdo_ :: El_
bdo_ = el_ "bdo"

bdo :: El
bdo = el "bdo"

bdo' :: El'
bdo' = el' "bdo"

blockquote_ :: El_
blockquote_ = el_ "blockquote"

blockquote :: El
blockquote = el "blockquote"

blockquote' :: El'
blockquote' = el' "blockquote"

body_ :: El_
body_ = el_ "body"

body :: El
body = el "body"

body' :: El'
body' = el' "body"

button_ :: El_
button_ = el_ "button"

button :: El
button = el "button"

button' :: El'
button' = el' "button"

canvas_ :: El_
canvas_ = el_ "canvas"

canvas :: El
canvas = el "canvas"

canvas' :: El'
canvas' = el' "canvas"

caption_ :: El_
caption_ = el_ "caption"

caption :: El
caption = el "caption"

caption' :: El'
caption' = el' "caption"

cite_ :: El_
cite_ = el_ "cite"

cite :: El
cite = el "cite"

cite' :: El'
cite' = el' "cite"

code_ :: El_
code_ = el_ "code"

code :: El
code = el "code"

code' :: El'
code' = el' "code"

col_ :: El_
col_ = el_ "col"

col :: El
col = el "col"

col' :: El'
col' = el' "col"

colgroup_ :: El_
colgroup_ = el_ "colgroup"

colgroup :: El
colgroup = el "colgroup"

colgroup' :: El'
colgroup' = el' "colgroup"

data_ :: El
data_ = el "data"

data' :: El'
data' = el' "data"

datalist_ :: El_
datalist_ = el_ "datalist"

datalist :: El
datalist = el "datalist"

datalist' :: El'
datalist' = el' "datalist"

dd_ :: El_
dd_ = el_ "dd"

dd :: El
dd = el "dd"

dd' :: El'
dd' = el' "dd"

del_ :: El_
del_ = el_ "del"

del :: El
del = el "del"

del' :: El'
del' = el' "del"

details_ :: El_
details_ = el_ "details"

details :: El
details = el "details"

details' :: El'
details' = el' "details"

dfn_ :: El_
dfn_ = el_ "dfn"

dfn :: El
dfn = el "dfn"

dfn' :: El'
dfn' = el' "dfn"

dialog_ :: El_
dialog_ = el_ "dialog"

dialog :: El
dialog = el "dialog"

dialog' :: El'
dialog' = el' "dialog"

div_ :: El_
div_ = el_ "div"

div :: El
div = el "div"

div' :: El'
div' = el' "div"

dl_ :: El_
dl_ = el_ "dl"

dl :: El
dl = el "dl"

dl' :: El'
dl' = el' "dl"

dt_ :: El_
dt_ = el_ "dt"

dt :: El
dt = el "dt"

dt' :: El'
dt' = el' "dt"

em_ :: El_
em_ = el_ "em"

em :: El
em = el "em"

em' :: El'
em' = el' "em"

embed_ :: El_
embed_ = el_ "embed"

embed :: El
embed = el "embed"

embed' :: El'
embed' = el' "embed"

fieldset_ :: El_
fieldset_ = el_ "fieldset"

fieldset :: El
fieldset = el "fieldset"

fieldset' :: El'
fieldset' = el' "fieldset"

figcaption_ :: El_
figcaption_ = el_ "figcaption"

figcaption :: El
figcaption = el "figcaption"

figcaption' :: El'
figcaption' = el' "figcaption"

figure_ :: El_
figure_ = el_ "figure"

figure :: El
figure = el "figure"

figure' :: El'
figure' = el' "figure"

footer_ :: El_
footer_ = el_ "footer"

footer :: El
footer = el "footer"

footer' :: El'
footer' = el' "footer"

form_ :: El_
form_ = el_ "form"

form :: El
form = el "form"

form' :: El'
form' = el' "form"

h1_ :: El_
h1_ = el_ "h1"

h1 :: El
h1 = el "h1"

h1' :: El'
h1' = el' "h1"

h2_ :: El_
h2_ = el_ "h2"

h2 :: El
h2 = el "h2"

h2' :: El'
h2' = el' "h2"

h3_ :: El_
h3_ = el_ "h3"

h3 :: El
h3 = el "h3"

h3' :: El'
h3' = el' "h3"

h4_ :: El_
h4_ = el_ "h4"

h4 :: El
h4 = el "h4"

h4' :: El'
h4' = el' "h4"

h5_ :: El_
h5_ = el_ "h5"

h5 :: El
h5 = el "h5"

h5' :: El'
h5' = el' "h5"

h6_ :: El_
h6_ = el_ "h6"

h6 :: El
h6 = el "h6"

h6' :: El'
h6' = el' "h6"

head_ :: El_
head_ = el_ "head"

head :: El
head = el "head"

head' :: El'
head' = el' "head"

header_ :: El_
header_ = el_ "header"

header :: El
header = el "header"

header' :: El'
header' = el' "header"

hgroup_ :: El_
hgroup_ = el_ "hgroup"

hgroup :: El
hgroup = el "hgroup"

hgroup' :: El'
hgroup' = el' "hgroup"

hr_ :: El_
hr_ = el_ "hr"

hr :: El
hr = el "hr"

hr' :: El'
hr' = el' "hr"

html_ :: El_
html_ = el_ "html"

html :: El
html = el "html"

html' :: El'
html' = el' "html"

i_ :: El_
i_ = el_ "i"

i :: El
i = el "i"

i' :: El'
i' = el' "i"

iframe_ :: El_
iframe_ = el_ "iframe"

iframe :: El
iframe = el "iframe"

iframe' :: El'
iframe' = el' "iframe"

ins_ :: El_
ins_ = el_ "ins"

ins :: El
ins = el "ins"

ins' :: El'
ins' = el' "ins"

kbd_ :: El_
kbd_ = el_ "kbd"

kbd :: El
kbd = el "kbd"

kbd' :: El'
kbd' = el' "kbd"

label_ :: El_
label_ = el_ "label"

label :: El
label = el "label"

label' :: El'
label' = el' "label"

legend_ :: El_
legend_ = el_ "legend"

legend :: El
legend = el "legend"

legend' :: El'
legend' = el' "legend"

li_ :: El_
li_ = el_ "li"

li :: El
li = el "li"

li' :: El'
li' = el' "li"

link_ :: El_
link_ = el_ "link"

link :: El
link = el "link"

link' :: El'
link' = el' "link"

main_ :: El_
main_ = el_ "main"

main :: El
main = el "main"

main' :: El'
main' = el' "main"

map_ :: El
map_ = el "map"

map' :: El'
map' = el' "map"

mark_ :: El_
mark_ = el_ "mark"

mark :: El
mark = el "mark"

mark' :: El'
mark' = el' "mark"

math_ :: El_
math_ = el_ "math"

math :: El
math = el "math"

math' :: El'
math' = el' "math"

menu_ :: El_
menu_ = el_ "menu"

menu :: El
menu = el "menu"

menu' :: El'
menu' = el' "menu"

menuitem_ :: El_
menuitem_ = el_ "menuitem"

menuitem :: El
menuitem = el "menuitem"

menuitem' :: El'
menuitem' = el' "menuitem"

meta_ :: El_
meta_ = el_ "meta"

meta :: El
meta = el "meta"

meta' :: El'
meta' = el' "meta"

meter_ :: El_
meter_ = el_ "meter"

meter :: El
meter = el "meter"

meter' :: El'
meter' = el' "meter"

nav_ :: El_
nav_ = el_ "nav"

nav :: El
nav = el "nav"

nav' :: El'
nav' = el' "nav"

noscript_ :: El_
noscript_ = el_ "noscript"

noscript :: El
noscript = el "noscript"

noscript' :: El'
noscript' = el' "noscript"

object_ :: El_
object_ = el_ "object"

object :: El
object = el "object"

object' :: El'
object' = el' "object"

ol_ :: El_
ol_ = el_ "ol"

ol :: El
ol = el "ol"

ol' :: El'
ol' = el' "ol"

optgroup_ :: El_
optgroup_ = el_ "optgroup"

optgroup :: El
optgroup = el "optgroup"

optgroup' :: El'
optgroup' = el' "optgroup"

option_ :: El_
option_ = el_ "option"

option :: El
option = el "option"

option' :: El'
option' = el' "option"

output_ :: El_
output_ = el_ "output"

output :: El
output = el "output"

output' :: El'
output' = el' "output"

p_ :: El_
p_ = el_ "p"

p :: El
p = el "p"

p' :: El'
p' = el' "p"

param_ :: El_
param_ = el_ "param"

param :: El
param = el "param"

param' :: El'
param' = el' "param"

picture_ :: El_
picture_ = el_ "picture"

picture :: El
picture = el "picture"

picture' :: El'
picture' = el' "picture"

pre_ :: El_
pre_ = el_ "pre"

pre :: El
pre = el "pre"

pre' :: El'
pre' = el' "pre"

progress_ :: El_
progress_ = el_ "progress"

progress :: El
progress = el "progress"

progress' :: El'
progress' = el' "progress"

q_ :: El_
q_ = el_ "q"

q :: El
q = el "q"

q' :: El'
q' = el' "q"

rb_ :: El_
rb_ = el_ "rb"

rb :: El
rb = el "rb"

rb' :: El'
rb' = el' "rb"

rp_ :: El_
rp_ = el_ "rp"

rp :: El
rp = el "rp"

rp' :: El'
rp' = el' "rp"

rt_ :: El_
rt_ = el_ "rt"

rt :: El
rt = el "rt"

rt' :: El'
rt' = el' "rt"

rtc_ :: El_
rtc_ = el_ "rtc"

rtc :: El
rtc = el "rtc"

rtc' :: El'
rtc' = el' "rtc"

ruby_ :: El_
ruby_ = el_ "ruby"

ruby :: El
ruby = el "ruby"

ruby' :: El'
ruby' = el' "ruby"

s_ :: El_
s_ = el_ "s"

s :: El
s = el "s"

s' :: El'
s' = el' "s"

samp_ :: El_
samp_ = el_ "samp"

samp :: El
samp = el "samp"

samp' :: El'
samp' = el' "samp"

script_ :: El_
script_ = el_ "script"

script :: El
script = el "script"

script' :: El'
script' = el' "script"

section_ :: El_
section_ = el_ "section"

section :: El
section = el "section"

section' :: El'
section' = el' "section"

select_ :: El_
select_ = el_ "select"

select :: El
select = el "select"

select' :: El'
select' = el' "select"

slot_ :: El_
slot_ = el_ "slot"

slot :: El
slot = el "slot"

slot' :: El'
slot' = el' "slot"

small_ :: El_
small_ = el_ "small"

small :: El
small = el "small"

small' :: El'
small' = el' "small"

source_ :: El_
source_ = el_ "source"

source :: El
source = el "source"

source' :: El'
source' = el' "source"

span_ :: El_
span_ = el_ "span"

span :: El
span = el "span"

span' :: El'
span' = el' "span"

strong_ :: El_
strong_ = el_ "strong"

strong :: El
strong = el "strong"

strong' :: El'
strong' = el' "strong"

style_ :: El_
style_ = el_ "style"

style :: El
style = el "style"

style' :: El'
style' = el' "style"

sub_ :: El_
sub_ = el_ "sub"

sub :: El
sub = el "sub"

sub' :: El'
sub' = el' "sub"

summary_ :: El_
summary_ = el_ "summary"

summary :: El
summary = el "summary"

summary' :: El'
summary' = el' "summary"

sup_ :: El_
sup_ = el_ "sup"

sup :: El
sup = el "sup"

sup' :: El'
sup' = el' "sup"

svg_ :: El_
svg_ = el_ "svg"

svg :: El
svg = el "svg"

svg' :: El'
svg' = el' "svg"

table_ :: El_
table_ = el_ "table"

table :: El
table = el "table"

table' :: El'
table' = el' "table"

tbody_ :: El_
tbody_ = el_ "tbody"

tbody :: El
tbody = el "tbody"

tbody' :: El'
tbody' = el' "tbody"

td_ :: El_
td_ = el_ "td"

td :: El
td = el "td"

td' :: El'
td' = el' "td"

template_ :: El_
template_ = el_ "template"

template :: El
template = el "template"

template' :: El'
template' = el' "template"

textarea_ :: El_
textarea_ = el_ "textarea"

textarea :: El
textarea = el "textarea"

textarea' :: El'
textarea' = el' "textarea"

tfoot_ :: El_
tfoot_ = el_ "tfoot"

tfoot :: El
tfoot = el "tfoot"

tfoot' :: El'
tfoot' = el' "tfoot"

th_ :: El_
th_ = el_ "th"

th :: El
th = el "th"

th' :: El'
th' = el' "th"

thead_ :: El_
thead_ = el_ "thead"

thead :: El
thead = el "thead"

thead' :: El'
thead' = el' "thead"

time_ :: El_
time_ = el_ "time"

time :: El
time = el "time"

time' :: El'
time' = el' "time"

title_ :: El_
title_ = el_ "title"

title :: El
title = el "title"

title' :: El'
title' = el' "title"

tr_ :: El_
tr_ = el_ "tr"

tr :: El
tr = el "tr"

tr' :: El'
tr' = el' "tr"

track_ :: El_
track_ = el_ "track"

track :: El
track = el "track"

track' :: El'
track' = el' "track"

u_ :: El_
u_ = el_ "u"

u :: El
u = el "u"

u' :: El'
u' = el' "u"

ul_ :: El_
ul_ = el_ "ul"

ul :: El
ul = el "ul"

ul' :: El'
ul' = el' "ul"

var_ :: El_
var_ = el_ "var"

var :: El
var = el "var"

var' :: El'
var' = el' "var"

video_ :: El_
video_ = el_ "video"

video :: El
video = el "video"

video' :: El'
video' = el' "video"

wbr_ :: El_
wbr_ = el_ "wbr"

wbr :: El
wbr = el "wbr"

wbr' :: El'
wbr' = el' "wbr"

img :: ElAttrs
img = elAttrs "img"

input :: ElAttrs
input = elAttrs "input"

br :: forall event. Html event
br = el "br" [] []

area :: ElAttrs
area = elAttrs "area"
