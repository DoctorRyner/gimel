module Gimel.Attributes where

import Prelude

import Effect.Aff (Aff, launchAff_)
import Effect.Uncurried (mkEffectFn1)
import Gimel.Cmd (Cmd(..))
import React.DOM.Props (Props, unsafeMkProps)
import React.SyntheticEvent (SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

data Attribute event
  = Attribute Props
  | AttributeEvent String (SyntheticEvent -> Cmd event)

instance functorAttribute :: Functor Attribute where
  map f = case _ of
    Attribute      props -> Attribute props
    AttributeEvent str e -> AttributeEvent str $ (map f) <<< e

on :: forall event. String -> event -> Attribute event
on eventName e = AttributeEvent ("on" <> eventName) $ const (Cmd \runEvent -> runEvent e)

on_ :: forall event. String -> Cmd event -> Attribute event
on_ eventName = AttributeEvent ("on" <> eventName) <<< const

on' :: forall event. String -> (SyntheticEvent -> Cmd event) -> Attribute event
on' eventName = AttributeEvent ("on" <> eventName)

attribute :: forall event propValue. String -> propValue -> Attribute event
attribute k v = Attribute $ unsafeMkProps k v

infix 4 attribute as =:

toReactProp :: forall event. (event -> Aff Unit) -> Attribute event -> Props
toReactProp runEvent (AttributeEvent eventName eventF) =
  unsafeMkProps eventName $ mkEffectFn1 (launchAff_ <<< (\(Cmd f) -> f runEvent) <<< eventF)
toReactProp _ (Attribute prop) = prop

-- Events

targetOf :: forall a. SyntheticEvent -> a
targetOf e = (unsafeCoerce e).target

currentTargetOf :: forall a. SyntheticEvent -> a
currentTargetOf e = (unsafeCoerce e).target

onClick :: forall event. event -> Attribute event
onClick = on "Click"

onDoubleClick :: forall event. event -> Attribute event
onDoubleClick = on "DoubleClick"

onMouseDown :: forall event. event -> Attribute event
onMouseDown = on "MouseDown"

onMouseUp :: forall event. event -> Attribute event
onMouseUp = on "MouseUp"

onMouseEnter :: forall event. event -> Attribute event
onMouseEnter = on "MouseEnter"

onMouseLeave :: forall event. event -> Attribute event
onMouseLeave = on "MouseLeave"

onMouseOver :: forall event. event -> Attribute event
onMouseOver = on "MouseOver"

onMouseOut :: forall event. event -> Attribute event
onMouseOut = on "MouseOut"

-- onChange :: forall event. (String -> event) -> Attribute event
-- onChange f = on' "Change" \e -> f (targetOf e).value

-- onInput :: forall event. (String -> event) -> Attribute event
-- onInput f = on' "Input" \e -> f (targetOf e).value

-- onCheck :: forall event. (Boolean -> event) -> Attribute event
-- onCheck f = on' "Check" \e -> f (targetOf e).checked

onSubmit :: forall event. event -> Attribute event
onSubmit = on "Submit"

onBlur :: forall event. event -> Attribute event
onBlur = on "Blur"

onFocus :: forall event. event -> Attribute event
onFocus = on "Focus"

-- Attributes

_data :: forall a event. Record a -> Attribute event
_data = attribute "data"

style :: forall a event. Record a -> Attribute event
style = attribute "style"

accept :: forall event. String -> Attribute event
accept = attribute "accept"

acceptCharset :: forall event. String -> Attribute event
acceptCharset = attribute "accept-charset"

accessKey :: forall event. String -> Attribute event
accessKey = attribute "accesskey"

allowFullScreen :: forall event. Boolean -> Attribute event
allowFullScreen = attribute "allowfullscreen"

allowTransparency :: forall event. Boolean -> Attribute event
allowTransparency = attribute "allowTransparency"

alt :: forall event. String -> Attribute event
alt = attribute "alt"

async :: forall event. Boolean -> Attribute event
async = attribute "async"

autoComplete :: forall event. String -> Attribute event
autoComplete = attribute "autocomplete"

autoFocus :: forall event. Boolean -> Attribute event
autoFocus = attribute "autofocus"

autoPlay :: forall event. Boolean -> Attribute event
autoPlay = attribute "autoplay"

capture :: forall event. Boolean -> Attribute event
capture = attribute "capture"

cellPadding :: forall event. String -> Attribute event
cellPadding = attribute "cellpadding"

cellSpacing :: forall event. String -> Attribute event
cellSpacing = attribute "cellspacing"

charset :: forall event. String -> Attribute event
charset = attribute "charset"

challenge :: forall event. Boolean -> Attribute event
challenge = attribute "challenge"

checked :: forall event. Boolean -> Attribute event
checked = attribute "checked"

cite :: forall event. String -> Attribute event
cite = attribute "cite"

classID :: forall event. String -> Attribute event
classID = attribute "classid"

className :: forall event. String -> Attribute event
className = attribute "className"

cols :: forall event. Int -> Attribute event
cols = attribute "cols"

colspan :: forall event. Int -> Attribute event
colspan = attribute "colspan"

content :: forall event. String -> Attribute event
content = attribute "content"

contentEditable :: forall event. Boolean -> Attribute event
contentEditable = attribute "contenteditable"

contextMenu :: forall event. String -> Attribute event
contextMenu = attribute "contextmenu"

controls :: forall event. Boolean -> Attribute event
controls = attribute "controls"

coords :: forall event. String -> Attribute event
coords = attribute "coords"

crossorigin :: forall event. String -> Attribute event
crossorigin = attribute "crossorigin"

dateTime :: forall event. String -> Attribute event
dateTime = attribute "datetime"

default :: forall event. Boolean -> Attribute event
default = attribute "default"

defaultChecked :: forall event. Boolean -> Attribute event
defaultChecked = attribute "defaultChecked"

defaultValue :: forall event. Boolean -> Attribute event
defaultValue = attribute "defaultValue"

defer :: forall event. Boolean -> Attribute event
defer = attribute "defer"

dir :: forall event. String -> Attribute event
dir = attribute "dir"

disabled :: forall event. Boolean -> Attribute event
disabled = attribute "disabled"

download :: forall event. String -> Attribute event
download = attribute "download"

draggable :: forall event. Boolean -> Attribute event
draggable = attribute "draggable"

encType :: forall event. String -> Attribute event
encType = attribute "enctype"

form :: forall event. String -> Attribute event
form = attribute "form"

formAction :: forall event. String -> Attribute event
formAction = attribute "formaction"

formEncType :: forall event. String -> Attribute event
formEncType = attribute "formenctype"

formMethod :: forall event. String -> Attribute event
formMethod = attribute "formmethod"

formNoValidate :: forall event. Boolean -> Attribute event
formNoValidate = attribute "formnovalidate"

formTarget :: forall event. String -> Attribute event
formTarget = attribute "formtarget"

headers :: forall event. String -> Attribute event
headers = attribute "headers"

height :: forall event. String -> Attribute event
height = attribute "height"

hidden :: forall event. Boolean -> Attribute event
hidden = attribute "hidden"

high :: forall event. String -> Attribute event
high = attribute "high"

href :: forall event. String -> Attribute event
href = attribute "href"

hrefLang :: forall event. String -> Attribute event
hrefLang = attribute "hreflang"

htmlFor :: forall event. String -> Attribute event
htmlFor = attribute "htmlfor"

httpEquiv :: forall event. String -> Attribute event
httpEquiv = attribute "httpequiv"

icon :: forall event. String -> Attribute event
icon = attribute "icon"

_id :: forall event. String -> Attribute event
_id = attribute "id"

inputMode :: forall event. String -> Attribute event
inputMode = attribute "inputmode"

integrity :: forall event. String -> Attribute event
integrity = attribute "integrity"

is :: forall event. String -> Attribute event
is = attribute "is"

key :: forall event. String -> Attribute event
key = attribute "key"

keyparams :: forall event. String -> Attribute event
keyparams = attribute "keyparams"

type_ :: forall event. String -> Attribute event
type_ = attribute "type"

keytype :: forall event. String -> Attribute event
keytype = attribute "keytype"

kind :: forall event. String -> Attribute event
kind = attribute "kind"

label :: forall event. String -> Attribute event
label = attribute "label"

list :: forall event. String -> Attribute event
list = attribute "list"

lang :: forall event. String -> Attribute event
lang = attribute "lang"

loop :: forall event. Boolean -> Attribute event
loop = attribute "loop"

low :: forall event. String -> Attribute event
low = attribute "low"

manifest :: forall event. String -> Attribute event
manifest = attribute "manifest"

marginHeight :: forall event. String -> Attribute event
marginHeight = attribute "marginheight"

marginWidth :: forall event. String -> Attribute event
marginWidth = attribute "marginwidth"

max :: forall event. String -> Attribute event
max = attribute "max"

maxLenght :: forall event. String -> Attribute event
maxLenght = attribute "maxlenght"

media :: forall event. String -> Attribute event
media = attribute "media"

mediaGroup :: forall event. String -> Attribute event
mediaGroup = attribute "media"

method :: forall event. String -> Attribute event
method = attribute "method"

min :: forall event. String -> Attribute event
min = attribute "min"

minLength :: forall event. String -> Attribute event
minLength = attribute "minlenght"

muted :: forall event. Boolean -> Attribute event
muted = attribute "muted"

multiple :: forall event. Boolean -> Attribute event
multiple = attribute "multiple"

name :: forall event. String -> Attribute event
name = attribute "name"

nonce :: forall event. String -> Attribute event
nonce = attribute "nonce"

noValidate :: forall event. Boolean -> Attribute event
noValidate = attribute "novalidate"

open :: forall event. Boolean -> Attribute event
open = attribute "open"

optimum :: forall event. String -> Attribute event
optimum = attribute "optimum"

pattern :: forall event. String -> Attribute event
pattern = attribute "pattern"

placeholder :: forall event. String -> Attribute event
placeholder = attribute "placeholder"

poster :: forall event. String -> Attribute event
poster = attribute "poster"

preload :: forall event. String -> Attribute event
preload = attribute "preload"

profile :: forall event. String -> Attribute event
profile = attribute "profile"

radioGroup :: forall event. String -> Attribute event
radioGroup = attribute "radiogroup"

readOnly :: forall event. Boolean -> Attribute event
readOnly = attribute "readonly"

rel :: forall event. String -> Attribute event
rel = attribute "rel"

required :: forall event. Boolean -> Attribute event
required = attribute "required"

value :: forall event. String -> Attribute event
value = attribute "value"
