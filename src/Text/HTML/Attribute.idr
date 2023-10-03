module Text.HTML.Attribute

import Data.List
import Data.Maybe
import Data.String
import Text.HTML.Event
import Text.HTML.Tag
import Text.HTML.Ref

%default total

public export
data Dir = LTR | RTL

export
Show Dir where
  show LTR = "ltr"
  show RTL = "rtl"

public export
data LoadType = Lzy | Eager

export
Show LoadType where
  show Lzy   = "lazy"
  show Eager = "eager"

||| Enum representing different types of input elements
public export
data InputType =
    Button
  | CheckBox
  | Color
  | Date
  | DateTime
  | Email
  | File
  | Image
  | Month
  | Number
  | Password
  | Radio
  | Range
  | Tel
  | Text
  | Time
  | URL
  | Week

export
Show InputType where
  show Button   = "button"
  show CheckBox = "checkbox"
  show Color    = "color"
  show Date     = "date"
  show DateTime = "datetime-local"
  show Email    = "email"
  show File     = "file"
  show Image    = "image"
  show Month    = "month"
  show Number   = "number"
  show Password = "password"
  show Radio    = "radio"
  show Range    = "range"
  show Tel      = "tel"
  show Text     = "text"
  show Time     = "time"
  show URL      = "url"
  show Week     = "week"

||| An attribute indexed by the `HTMLTag` used for the element
||| in question.
|||
||| This allows us to make sure we don't use invalid `Ref`s (which can
||| be later used to retrieve an element from the DOM) in a HTML node.
public export
data Attribute : {0 k : Type} -> (t : k) -> (event : Type) -> Type where
  Id     : {0 t : HTMLTag s} -> Ref t -> Attribute t event
  Str    : (name : String) -> (value : String) -> Attribute t event
  Bool   : (name : String) -> (value : Bool) -> Attribute t event
  Event_ :
       (preventDefault, stopPropagation : Bool)
    -> DOMEvent event
    -> Attribute t event
  Empty  : Attribute t event

export
Functor (Attribute t) where
  map f (Id x)           = Id x
  map f (Str n v)        = Str n v
  map f (Bool n v)       = Bool n v
  map f (Event_ pd sp e) = Event_ pd sp $ map f e
  map f Empty            = Empty

public export
Attributes : {0 k : _} -> (t : k) -> Type -> Type
Attributes t e = List (Attribute t e)

export %inline
Event : DOMEvent ev -> Attribute t ev
Event = Event_ False False

export
displayAttribute : Attribute t ev -> Maybe String
displayAttribute (Id (Id va))   = Just #"id="\#{va}""#
displayAttribute (Str nm va)    = Just #"\#{nm}="\#{va}""#
displayAttribute (Bool nm True) = Just nm
displayAttribute (Bool _ False) = Nothing
displayAttribute (Event_ _ _ _) = Nothing
displayAttribute Empty          = Nothing

export
displayAttributes : Attributes t ev -> String
displayAttributes = fastConcat . intersperse " " . mapMaybe displayAttribute

export
dispAttr : String -> (a -> String) -> a -> Attribute t ev
dispAttr nm f =  Str nm . f

export
showAttr : Show a => String -> a -> Attribute t ev
showAttr nm = dispAttr nm show

export %inline
accesskey : String -> Attribute t ev
accesskey = Str "accesskey"

export %inline
action : String -> Attribute t ev
action = Str "action"

export %inline
alt : String -> Attribute t ev
alt = Str "alt"

export %inline
autocapitalize : Bool -> Attribute t ev
autocapitalize = Bool "autocapitalize"

export %inline
autocomplete : Bool -> Attribute t ev
autocomplete = Bool "autocomplete"

export %inline
autofocus : Bool -> Attribute t ev
autofocus = Bool "autofocus"

export %inline
autoplay : Bool -> Attribute t ev
autoplay = Bool "autoplay"

export %inline
checked : Bool -> Attribute t ev
checked = Bool "checked"

export %inline
cite : String -> Attribute t ev
cite = Str "cite"

export %inline
class : String -> Attribute t ev
class = Str "class"

export %inline
classes : List String -> Attribute t ev
classes = dispAttr "class" (fastConcat . intersperse " ")

export %inline
cols : Bits32 -> Attribute t ev
cols = showAttr "cols"

export %inline
colspan : Bits32 -> Attribute t ev
colspan = showAttr "colspan"

export %inline
contenteditable : Bool -> Attribute t ev
contenteditable = Bool "contenteditable"

export %inline
controls : Bool -> Attribute t ev
controls = Bool "controls"

export %inline
data_ : String -> Attribute t ev
data_ = Str "data"

export %inline
dir : Dir -> Attribute t ev
dir = showAttr "dir"

export %inline
disabled : Bool -> Attribute t ev
disabled = Bool "disabled"

export %inline
download : String -> Attribute t ev
download = Str "download"

export %inline
draggable : Bool -> Attribute t ev
draggable = Bool "draggable"

export %inline
for : String -> Attribute t ev
for = Str "for"

export %inline
form : String -> Attribute t ev
form = Str "form"

export %inline
height : Bits32 -> Attribute t ev
height = showAttr "height"

export %inline
hidden : Bool -> Attribute t ev
hidden = Bool "hidden"

export %inline
href : String -> Attribute t ev
href = Str "href"

export %inline
hreflang : String -> Attribute t ev
hreflang = Str "hreflang"

export %inline
id : String -> Attribute t ev
id = Str "id"

export %inline
label : String -> Attribute t ev
label = Str "label"

export %inline
lang : String -> Attribute t ev
lang = Str "lang"

export %inline
loading : LoadType -> Attribute t ev
loading = showAttr "loading"

export %inline
list : String -> Attribute t ev
list = Str "list"

export %inline
loop : Bool -> Attribute t ev
loop = Bool "loop"

export %inline
maxlength : Bits32 -> Attribute t ev
maxlength = showAttr "maxlength"

export %inline
minlength : Bits32 -> Attribute t ev
minlength = showAttr "minlength"

export %inline
multiple : Bool -> Attribute t ev
multiple = Bool "multiple"

export %inline
muted : Bool -> Attribute t ev
muted = Bool "muted"

export %inline
name : String -> Attribute t ev
name = Str "name"

export %inline
placeholder : String -> Attribute t ev
placeholder = Str "placeholder"

export %inline
readonly : Bool -> Attribute t ev
readonly = Bool "readonly"

export %inline
required : Bool -> Attribute t ev
required = Bool "required"

export %inline
reverse : Bool -> Attribute t ev
reverse = Bool "reverse"

export %inline
rows : Bits32 -> Attribute t ev
rows = showAttr "rows"

export %inline
rowspan : Bits32 -> Attribute t ev
rowspan = showAttr "rowspan"

export %inline
selected : Bool -> Attribute t ev
selected = Bool "selected"

export %inline
spellcheck : Bool -> Attribute t ev
spellcheck = Bool "spellcheck"

export %inline
src : String -> Attribute t ev
src = Str "src"

export %inline
style : String -> Attribute t ev
style = Str "style"

export %inline
tabindex : Int32 -> Attribute t ev
tabindex = showAttr "tabindex"

export %inline
target : String -> Attribute t ev
target = Str "target"

export %inline
title : String -> Attribute t ev
title = Str "title"

export %inline
type : InputType -> Attribute t ev
type = showAttr "type"

export %inline
value : String -> Attribute t ev
value = Str "value"

export %inline
width : Bits32 -> Attribute t ev
width = showAttr "width"

export %inline
wrap : Bool -> Attribute t ev
wrap = Bool "wrap"

--------------------------------------------------------------------------------
--          Events
--------------------------------------------------------------------------------

export %inline
click : (MouseInfo -> Maybe ev) -> Attribute t ev
click = Event . Click

export %inline
onClick : ev -> Attribute t ev
onClick = click . const . Just

export
onLeftClick : ev -> Attribute t ev
onLeftClick va = click $ \mi => toMaybe (mi.button == 0) va

export
onRightClick : ev -> Attribute t ev
onRightClick va = click $ \mi => toMaybe (mi.button == 2) va

export
onMiddleClick : ev -> Attribute t ev
onMiddleClick va = click $ \mi => toMaybe (mi.button == 1) va

export %inline
dblClick : (MouseInfo -> Maybe ev) -> Attribute t ev
dblClick = Event . DblClick

export %inline
onDblClick : ev -> Attribute t ev
onDblClick = dblClick . const . Just

export %inline
onMouseEnter : ev -> Attribute t ev
onMouseEnter = Event . MouseEnter . const . Just

export %inline
onMouseLeave : ev -> Attribute t ev
onMouseLeave = Event . MouseLeave . const . Just

export %inline
onMouseOver : ev -> Attribute t ev
onMouseOver = Event . MouseOver . const . Just

export %inline
onMouseOut : ev -> Attribute t ev
onMouseOut = Event . MouseOut . const . Just

export
onChange : (String -> ev) -> Attribute t ev
onChange f = Event . Change $ Just . f . value

export
onChangeMaybe : (String -> Maybe ev) -> Attribute t ev
onChangeMaybe f = Event . Change $ f . value

export
onChecked : (Bool -> ev) -> Attribute t ev
onChecked f = Event . Change $ Just . f . checked

export
onInput : (String -> ev) -> Attribute t ev
onInput f = Event . Input $ Just . f . value

export
onEnterDown : ev -> Attribute t ev
onEnterDown va = Event . KeyDown $ \k => toMaybe (k.key == "Enter") va

export
onEscDown : ev -> Attribute t ev
onEscDown va = Event . KeyDown $ \k => toMaybe (k.key == "Escape") va

export
onKeyUp : (KeyInfo -> ev) -> Attribute t ev
onKeyUp f = Event . KeyUp $ Just . f

export
onBlur : ev -> Attribute t ev
onBlur = Event . Blur

export
onFocus : ev -> Attribute t ev
onFocus = Event . Focus
