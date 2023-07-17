module Text.HTML.Attribute

import Data.List
import Data.Maybe
import Data.String
import Text.HTML.Event

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

public export
data Attribute : (event : Type) -> Type where
  Id    : (value : String) -> Attribute event
  Str   : (name : String) -> (value : String) -> Attribute event
  Bool  : (name : String) -> (value : Bool) -> Attribute event
  Event : DOMEvent event -> Attribute event

public export
Attributes : Type -> Type
Attributes = List . Attribute

export
displayAttribute : Attribute ev -> Maybe String
displayAttribute (Id va)        = Just #"id="\#{va}""#
displayAttribute (Str nm va)    = Just #"\#{nm}="\#{va}""#
displayAttribute (Bool nm True) = Just nm
displayAttribute (Bool _ False) = Nothing
displayAttribute (Event _)      = Nothing

export
displayAttributes : Attributes ev -> String
displayAttributes = fastConcat . intersperse " " . mapMaybe displayAttribute

export
getId : Attributes ev -> Maybe String
getId (Id v       :: _) = Just v
getId (Str "id" v :: t) = Just v
getId (_          :: t) = getId t
getId []                = Nothing

export
getEvents : Attributes ev -> List (DOMEvent ev)
getEvents = go Nil
  where go : List (DOMEvent ev) -> Attributes ev -> List (DOMEvent ev)
        go es []              = es
        go es (Event e :: xs) = go (e :: es) xs
        go es (_ :: xs)       = go es xs

export
dispAttr : String -> (a -> String) -> a -> Attribute ev
dispAttr nm f =  Str nm . f

export
showAttr : Show a => String -> a -> Attribute ev
showAttr nm = dispAttr nm show

export %inline
accesskey : String -> Attribute ev
accesskey = Str "accesskey"

export %inline
action : String -> Attribute ev
action = Str "action"

export %inline
alt : String -> Attribute ev
alt = Str "alt"

export %inline
autocapitalize : Bool -> Attribute ev
autocapitalize = Bool "autocapitalize"

export %inline
autocomplete : Bool -> Attribute ev
autocomplete = Bool "autocomplete"

export %inline
autofocus : Bool -> Attribute ev
autofocus = Bool "autofocus"

export %inline
autoplay : Bool -> Attribute ev
autoplay = Bool "autoplay"

export %inline
checked : Bool -> Attribute ev
checked = Bool "checked"

export %inline
cite : String -> Attribute ev
cite = Str "cite"

export %inline
class : String -> Attribute ev
class = Str "class"

export %inline
classes : List String -> Attribute ev
classes = dispAttr "class" (fastConcat . intersperse " ")

export %inline
cols : Bits32 -> Attribute ev
cols = showAttr "cols"

export %inline
colspan : Bits32 -> Attribute ev
colspan = showAttr "colspan"

export %inline
contenteditable : Bool -> Attribute ev
contenteditable = Bool "contenteditable"

export %inline
controls : Bool -> Attribute ev
controls = Bool "controls"

export %inline
data_ : String -> Attribute ev
data_ = Str "data"

export %inline
dir : Dir -> Attribute ev
dir = showAttr "dir"

export %inline
disabled : Bool -> Attribute ev
disabled = Bool "disabled"

export %inline
download : String -> Attribute ev
download = Str "download"

export %inline
draggable : Bool -> Attribute ev
draggable = Bool "draggable"

export %inline
for : String -> Attribute ev
for = Str "for"

export %inline
form : String -> Attribute ev
form = Str "form"

export %inline
height : Bits32 -> Attribute ev
height = showAttr "height"

export %inline
hidden : Bool -> Attribute ev
hidden = Bool "hidden"

export %inline
href : String -> Attribute ev
href = Str "href"

export %inline
hreflang : String -> Attribute ev
hreflang = Str "hreflang"

export %inline
id : String -> Attribute ev
id = Id

export %inline
label : String -> Attribute ev
label = Str "label"

export %inline
lang : String -> Attribute ev
lang = Str "lang"

export %inline
loading : LoadType -> Attribute ev
loading = showAttr "loading"

export %inline
list : String -> Attribute ev
list = Str "list"

export %inline
loop : Bool -> Attribute ev
loop = Bool "loop"

export %inline
maxlength : Bits32 -> Attribute ev
maxlength = showAttr "maxlength"

export %inline
minlength : Bits32 -> Attribute ev
minlength = showAttr "minlength"

export %inline
multiple : Bool -> Attribute ev
multiple = Bool "multiple"

export %inline
muted : Bool -> Attribute ev
muted = Bool "muted"

export %inline
name : String -> Attribute ev
name = Str "name"

export %inline
placeholder : String -> Attribute ev
placeholder = Str "placeholder"

export %inline
readonly : Bool -> Attribute ev
readonly = Bool "readonly"

export %inline
required : Bool -> Attribute ev
required = Bool "required"

export %inline
reverse : Bool -> Attribute ev
reverse = Bool "reverse"

export %inline
rows : Bits32 -> Attribute ev
rows = showAttr "rows"

export %inline
rowspan : Bits32 -> Attribute ev
rowspan = showAttr "rowspan"

export %inline
selected : Bool -> Attribute ev
selected = Bool "selected"

export %inline
spellcheck : Bool -> Attribute ev
spellcheck = Bool "spellcheck"

export %inline
src : String -> Attribute ev
src = Str "src"

export %inline
style : String -> Attribute ev
style = Str "style"

export %inline
tabindex : Int32 -> Attribute ev
tabindex = showAttr "tabindex"

export %inline
target : String -> Attribute ev
target = Str "target"

export %inline
title : String -> Attribute ev
title = Str "title"

export %inline
type : InputType -> Attribute ev
type = showAttr "type"

export %inline
value : String -> Attribute ev
value = Str "value"

export %inline
width : Bits32 -> Attribute ev
width = showAttr "width"

export %inline
wrap : Bool -> Attribute ev
wrap = Bool "wrap"

--------------------------------------------------------------------------------
--          Events
--------------------------------------------------------------------------------

export %inline
click : (MouseInfo -> Maybe ev) -> Attribute ev
click = Event . Click

export %inline
onClick : ev -> Attribute ev
onClick = click . const . Just

export
onLeftClick : ev -> Attribute ev
onLeftClick va = click $ \mi => toMaybe (mi.button == 0) va

export
onRightClick : ev -> Attribute ev
onRightClick va = click $ \mi => toMaybe (mi.button == 2) va

export
onMiddleClick : ev -> Attribute ev
onMiddleClick va = click $ \mi => toMaybe (mi.button == 1) va

export %inline
dblClick : (MouseInfo -> Maybe ev) -> Attribute ev
dblClick = Event . DblClick

export %inline
onDblClick : ev -> Attribute ev
onDblClick = dblClick . const . Just

export
onChange : (String -> ev) -> Attribute ev
onChange f = Event . Change $ Just . f . value

export
onChecked : (Bool -> ev) -> Attribute ev
onChecked f = Event . Change $ Just . f . checked

export
onInput : (String -> ev) -> Attribute ev
onInput f = Event . Input $ Just . f . value

export
onEnterDown : ev -> Attribute ev
onEnterDown va = Event . KeyDown $ \k => toMaybe (k.key == "Enter") va

export
onEscDown : ev -> Attribute ev
onEscDown va = Event . KeyDown $ \k => toMaybe (k.key == "Escape") va

export
onKeyUp : (KeyInfo -> ev) -> Attribute ev
onKeyUp f = Event . KeyUp $ Just . f

export
onBlur : ev -> Attribute ev
onBlur = Event . Blur

export
onFocus : ev -> Attribute ev
onFocus = Event . Focus
