module Text.HTML.Node

import Data.String
import Text.HTML.Attribute
import Text.HTML.Event
-- import Web.Dom

%default total

export
data Node : (event : Type) -> Type where
  El    :  {0 ev : Type}
        -> (tag : String)
        -> List (Attribute ev)
        -> List (Node ev)
        -> Node ev

  Raw   : String -> Node ev

  Text  : String -> Node ev

  Empty : Node ev

export %inline
FromString (Node ev) where
  fromString = Text

export %inline
a : List (Attribute ev) -> List (Node ev) -> Node ev
a = El "a"

export %inline
address : List (Attribute ev) -> List (Node ev) -> Node ev
address = El "address"

export %inline
area : List (Attribute ev) -> List (Node ev) -> Node ev
area = El "area"

export %inline
article : List (Attribute ev) -> List (Node ev) -> Node ev
article = El "article"

export %inline
audio : List (Attribute ev) -> List (Node ev) -> Node ev
audio = El "audio"

export %inline
base : List (Attribute ev) -> List (Node ev) -> Node ev
base = El "base"

export %inline
blockquote : List (Attribute ev) -> List (Node ev) -> Node ev
blockquote = El "blockquote"

export %inline
body : List (Attribute ev) -> List (Node ev) -> Node ev
body = El "body"

export %inline
br : List (Attribute ev) -> List (Node ev) -> Node ev
br = El "br"

export %inline
button : List (Attribute ev) -> List (Node ev) -> Node ev
button = El "button"

export %inline
canvas : List (Attribute ev) -> List (Node ev) -> Node ev
canvas = El "canvas"

export %inline
caption : List (Attribute ev) -> List (Node ev) -> Node ev
caption = El "caption"

export %inline
col : List (Attribute ev) -> List (Node ev) -> Node ev
col = El "col"

export %inline
colgroup : List (Attribute ev) -> List (Node ev) -> Node ev
colgroup = El "colgroup"

export %inline
data_ : List (Attribute ev) -> List (Node ev) -> Node ev
data_ = El "data"

export %inline
datalist : List (Attribute ev) -> List (Node ev) -> Node ev
datalist = El "datalist"

export %inline
del : List (Attribute ev) -> List (Node ev) -> Node ev
del = El "del"

export %inline
details : List (Attribute ev) -> List (Node ev) -> Node ev
details = El "details"

export %inline
dialog : List (Attribute ev) -> List (Node ev) -> Node ev
dialog = El "dialog"

export %inline
div : List (Attribute ev) -> List (Node ev) -> Node ev
div = El "div"

export %inline
dl : List (Attribute ev) -> List (Node ev) -> Node ev
dl = El "dl"

export %inline
embed : List (Attribute ev) -> List (Node ev) -> Node ev
embed = El "embed"

export %inline
fieldset : List (Attribute ev) -> List (Node ev) -> Node ev
fieldset = El "fieldSet"

export %inline
footer : List (Attribute ev) -> List (Node ev) -> Node ev
footer = El "footer"

export %inline
form : List (Attribute ev) -> List (Node ev) -> Node ev
form = El "form"

export %inline
h1 : List (Attribute ev) -> List (Node ev) -> Node ev
h1 = El "h1"

export %inline
h2 : List (Attribute ev) -> List (Node ev) -> Node ev
h2 = El "h2"

export %inline
h3 : List (Attribute ev) -> List (Node ev) -> Node ev
h3 = El "h3"

export %inline
h4 : List (Attribute ev) -> List (Node ev) -> Node ev
h4 = El "h4"

export %inline
h5 : List (Attribute ev) -> List (Node ev) -> Node ev
h5 = El "h5"

export %inline
h6 : List (Attribute ev) -> List (Node ev) -> Node ev
h6 = El "h6"

export %inline
header : List (Attribute ev) -> List (Node ev) -> Node ev
header = El "header"

export %inline
hr : List (Attribute ev) -> List (Node ev) -> Node ev
hr = El "hr"

export %inline
html : List (Attribute ev) -> List (Node ev) -> Node ev
html = El "html"

export %inline
iframe : List (Attribute ev) -> List (Node ev) -> Node ev
iframe = El "iframe"

export %inline
img : List (Attribute ev) -> List (Node ev) -> Node ev
img = El "img"

export %inline
input : List (Attribute ev) -> List (Node ev) -> Node ev
input = El "input"

export %inline
ins : List (Attribute ev) -> List (Node ev) -> Node ev
ins = El "ins"

export %inline
label : List (Attribute ev) -> List (Node ev) -> Node ev
label = El "label"

export %inline
legend : List (Attribute ev) -> List (Node ev) -> Node ev
legend = El "legend"

export %inline
li : List (Attribute ev) -> List (Node ev) -> Node ev
li = El "li"

export %inline
link : List (Attribute ev) -> List (Node ev) -> Node ev
link = El "link"

export %inline
map : List (Attribute ev) -> List (Node ev) -> Node ev
map = El "map"

export %inline
menu : List (Attribute ev) -> List (Node ev) -> Node ev
menu = El "menu"

export %inline
meta : List (Attribute ev) -> List (Node ev) -> Node ev
meta = El "meta"

export %inline
meter : List (Attribute ev) -> List (Node ev) -> Node ev
meter = El "meter"

export %inline
object : List (Attribute ev) -> List (Node ev) -> Node ev
object = El "object"

export %inline
ol : List (Attribute ev) -> List (Node ev) -> Node ev
ol = El "ol"

export %inline
optgroup : List (Attribute ev) -> List (Node ev) -> Node ev
optgroup = El "optgroup"

export %inline
option : List (Attribute ev) -> List (Node ev) -> Node ev
option = El "option"

export %inline
output : List (Attribute ev) -> List (Node ev) -> Node ev
output = El "output"

export %inline
p : List (Attribute ev) -> List (Node ev) -> Node ev
p = El "p"

export %inline
param : List (Attribute ev) -> List (Node ev) -> Node ev
param = El "param"

export %inline
picture : List (Attribute ev) -> List (Node ev) -> Node ev
picture = El "picture"

export %inline
pre : List (Attribute ev) -> List (Node ev) -> Node ev
pre = El "pre"

export %inline
progress : List (Attribute ev) -> List (Node ev) -> Node ev
progress = El "progress"

export %inline
q : List (Attribute ev) -> List (Node ev) -> Node ev
q = El "q"

export %inline
script : List (Attribute ev) -> List (Node ev) -> Node ev
script = El "script"

export %inline
section : List (Attribute ev) -> List (Node ev) -> Node ev
section = El "section"

export %inline
select : List (Attribute ev) -> List (Node ev) -> Node ev
select = El "select"

export %inline
slot : List (Attribute ev) -> List (Node ev) -> Node ev
slot = El "slot"

export %inline
source : List (Attribute ev) -> List (Node ev) -> Node ev
source = El "source"

export %inline
span : List (Attribute ev) -> List (Node ev) -> Node ev
span = El "span"

export %inline
style : List (Attribute ev) -> List (Node ev) -> Node ev
style = El "style"

export %inline
table : List (Attribute ev) -> List (Node ev) -> Node ev
table = El "table"

export %inline
tbody : List (Attribute ev) -> List (Node ev) -> Node ev
tbody = El "tbody"

export %inline
td : List (Attribute ev) -> List (Node ev) -> Node ev
td = El "td"

export %inline
template : List (Attribute ev) -> List (Node ev) -> Node ev
template = El "template"

export %inline
textarea : List (Attribute ev) -> List (Node ev) -> Node ev
textarea = El "textarea"

export %inline
tfoot : List (Attribute ev) -> List (Node ev) -> Node ev
tfoot = El "tfoot"

export %inline
th : List (Attribute ev) -> List (Node ev) -> Node ev
th = El "th"

export %inline
thead : List (Attribute ev) -> List (Node ev) -> Node ev
thead = El "thead"

export %inline
time : List (Attribute ev) -> List (Node ev) -> Node ev
time = El "time"

export %inline
title : List (Attribute ev) -> List (Node ev) -> Node ev
title = El "title"

export %inline
tr : List (Attribute ev) -> List (Node ev) -> Node ev
tr = El "tr"

export %inline
track : List (Attribute ev) -> List (Node ev) -> Node ev
track = El "track"

export %inline
ul : List (Attribute ev) -> List (Node ev) -> Node ev
ul = El "ul"

export %inline
video : List (Attribute ev) -> List (Node ev) -> Node ev
video = El "video"

--------------------------------------------------------------------------------
--          Rendering Html
--------------------------------------------------------------------------------

export
escape : String -> String
escape = fastConcat . map esc . unpack
  where esc : Char -> String
        esc '<'          = "&lt;"
        esc '>'          = "&gt;"
        esc '&'          = "&amp;"
        esc '"'          = "&quot;"
        esc '\''         = "&#x27"
        esc '\n'         = "\n"
        esc '\r'         = "\r"
        esc '\t'         = "\t"
        esc c            = if c < ' ' then "" else singleton c

attrs : List (Attribute ev) -> String
attrs as = let s = displayAttributes as in if null s then "" else " " ++ s

export
render : Node ev -> String
render n = case n of
  Raw x         => x
  Text x        => escape x
  El tag as ns  => "<\{tag}\{attrs as}>\{go [<] ns}</\{tag}>"
  Empty         => ""

  where
    go : SnocList String -> List (Node ev) -> String
    go ss (n :: ns) = go (ss :< render n) ns
    go ss []        = concat $ ss <>> []

export
renderMany : List (Node ev) -> String
renderMany = fastConcat . map render
