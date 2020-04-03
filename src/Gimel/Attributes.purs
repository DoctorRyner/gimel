module Gimel.Attributes where

import Prelude

import Effect.Uncurried (mkEffectFn1)
import Gimel.Dispatcher (Dispatcher)
import React.DOM.Props (Props, unsafeMkProps)
import React.SyntheticEvent (SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

target :: forall a b. a -> b
target e = (unsafeCoerce e).target

data Attribute event
  = Attribute Props
  | ReactEvent String (SyntheticEvent -> event)

on :: forall event. String -> event -> Attribute event
on eventName = ReactEvent ("on" <> eventName) <<< const

on_ :: forall event. String -> (SyntheticEvent -> event) -> Attribute event
on_ eventName f = ReactEvent ("on" <> eventName) f

attribute :: forall event propValue. String -> propValue -> Attribute event
attribute k v = Attribute $ unsafeMkProps k v

type E = forall event. event -> Attribute event
type E_ a = forall event. (a -> event) -> Attribute event
type A a = forall event. a -> Attribute event
type AR = forall a event. Record a -> Attribute event

a :: forall a. String -> A a
a = attribute

infix 4 attribute as =:

toReactProp :: forall event. Dispatcher event -> Attribute event -> Props
toReactProp dispatch = case _ of
  Attribute prop -> prop
  ReactEvent eventName event -> unsafeMkProps eventName $ mkEffectFn1 (dispatch <<< event)

-- Events

onClick :: E
onClick = on "Click"

onChange :: E_ String
onChange f = on_ "Change" \e -> f $ (target e).value

onCheck :: E_ Boolean
onCheck f = on_ "Check" \e -> f $ (target e).checked

-- Attrs

_data :: AR
_data = a "data"

style :: AR
style = a "style"

accept :: A String
accept = a "accept"

acceptCharset :: A String
acceptCharset = a "accept-charset"

accessKey :: A String
accessKey = a "accesskey"

allowFullScreen :: A Boolean
allowFullScreen = a "allowfullscreen"

allowTransparency :: A Boolean
allowTransparency = a "allowTransparency"

alt :: A String
alt = a "alt"

async :: A Boolean
async = a "async"

autoComplete :: A String
autoComplete = a "autocomplete"

autoFocus :: A Boolean
autoFocus = a "autofocus"

autoPlay :: A Boolean
autoPlay = a "autoplay"

capture :: A Boolean
capture = a "capture"

cellPadding :: A String
cellPadding = a "cellpadding"

cellSpacing :: A String
cellSpacing = a "cellspacing"

charset :: A String
charset = a "charset"

challenge :: A Boolean
challenge = a "challenge"

checked :: A Boolean
checked = a "checked"

cite :: A String
cite = a "cite"

classID :: A String
classID = a "classid"

className :: A String
className = a "className"

cols :: A Int
cols = a "cols"

colspan :: A Int
colspan = a "colspan"

content :: A String
content = a "content"

contentEditable :: A Boolean
contentEditable = a "contenteditable"

contextMenu :: A String
contextMenu = a "contextmenu"

controls :: A Boolean
controls = a "controls"

coords :: A String
coords = a "coords"

crossorigin :: A String
crossorigin = a "crossorigin"

dateTime :: A String
dateTime = a "datetime"

default :: A Boolean
default = a "default"

defaultChecked :: A Boolean
defaultChecked = a "defaultChecked"

defaultValue :: A Boolean
defaultValue = a "defaultValue"

defer :: A Boolean
defer = a "defer"

dir :: A String
dir = a "dir"

disabled :: A Boolean
disabled = a "disabled"

download :: A String
download = a "download"

draggable :: A Boolean
draggable = a "draggable"

encType :: A String
encType = a "enctype"

form :: A String
form = a "form"

formAction :: A String
formAction = a "formaction"

formEncType :: A String
formEncType = a "formenctype"

formMethod :: A String
formMethod = a "formmethod"

formNoValidate :: A Boolean
formNoValidate = a "formnovalidate"

formTarget :: A String
formTarget = a "formtarget"

headers :: A String
headers = a "headers"

height :: A String
height = a "height"

hidden :: A Boolean
hidden = a "hidden"

high :: A String
high = a "high"

href :: A String
href = a "href"

hrefLang :: A String
hrefLang = a "hreflang"

htmlFor :: A String
htmlFor = a "htmlfor"

httpEquiv :: A String
httpEquiv = a "httpequiv"

icon :: A String
icon = a "icon"

_id :: A String
_id = a "id"

inputMode :: A String
inputMode = a "inputmode"

integrity :: A String
integrity = a "integrity"

is :: A String
is = a "is"

key :: A String
key = a "key"

keyparams :: A String
keyparams = a "keyparams"

keytype :: A String
keytype = a "keytype"

kind :: A String
kind = a "kind"

label :: A String
label = a "label"

list :: A String
list = a "list"

lang :: A String
lang = a "lang"

loop :: A Boolean
loop = a "loop"

low :: A String
low = a "low"

manifest :: A String
manifest = a "manifest"

marginHeight :: A String
marginHeight = a "marginheight"

marginWidth :: A String
marginWidth = a "marginwidth"

max :: A String
max = a "max"

maxLenght :: A String
maxLenght = a "maxlenght"

media :: A String
media = a "media"

mediaGroup :: A String
mediaGroup = a "media"

method :: A String
method = a "method"

min :: A String
min = a "min"

minLength :: A String
minLength = a "minlenght"

muted :: A Boolean
muted = a "muted"

multiple :: A Boolean
multiple = a "multiple"

name :: A String
name = a "name"

nonce :: A String
nonce = a "nonce"

noValidate :: A Boolean
noValidate = a "novalidate"

open :: A Boolean
open = a "open"

optimum :: A String
optimum = a "optimum"

pattern :: A String
pattern = a "pattern"

placeholder :: A String
placeholder = a "placeholder"

poster :: A String
poster = a "poster"

preload :: A String
preload = a "preload"

profile :: A String
profile = a "profile"

radioGroup :: A String
radioGroup = a "radiogroup"

readOnly :: A Boolean
readOnly = a "readonly"

rel :: A String
rel = a "rel"

required :: A Boolean
required = a "required"

value :: A String
value = a "value"
