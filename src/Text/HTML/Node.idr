module Text.HTML.Node

import JS
import Data.String
import Text.HTML.Attribute
import Text.HTML.Event
import Text.HTML.Ref
import Text.HTML.Tag

%default total

public export
data Node : (event : Type) -> Type where
  El    :  {0 ev  : Type}
        -> {tag   : String}
        -> (0 tpe : HTMLTag tag)
        -> List (Attribute tpe ev)
        -> List (Node ev)
        -> Node ev

  Raw   : String -> Node ev

  Text  : String -> Node ev

  Empty : Node ev

export %inline
FromString (Node ev) where
  fromString = Text

export
Functor Node where
  map f (El tpe xs ys) = El tpe (map f <$> xs) (assert_total $ map f <$> ys)
  map f (Raw str)      = Raw str
  map f (Text str)     = Text str
  map f Empty          = Empty

||| Prepend a non-event attribute to a node's list of attributes.
export
withAttribute :
     ({0 s : _} -> {0 t : HTMLTag s} -> Attribute t e)
  -> Node e
  -> Node e
withAttribute a (El tp as ns) = El tp (a ::as) ns
withAttribute a n             = n

||| Prepend the given ID to a node's list of attributes.
export
withId : String -> Node e -> Node e
withId s (El tp as ns) = El tp (Id (Id s) :: as) ns
withId s n             = n

||| Prepend the given event to a node's list of attributes.
export
withEv : DOMEvent e -> Node e -> Node e
withEv ev (El tp as ns) = El tp (Event ev :: as) ns
withEv ev n             = n

export %inline
a : List (Attribute A ev) -> List (Node ev) -> Node ev
a = El _

export %inline
address : List (Attribute Address ev) -> List (Node ev) -> Node ev
address = El _

export %inline
area : List (Attribute Area ev) -> List (Node ev) -> Node ev
area = El _

export %inline
article : List (Attribute Article ev) -> List (Node ev) -> Node ev
article = El _

export %inline
audio : List (Attribute Audio ev) -> List (Node ev) -> Node ev
audio = El _

export %inline
base : List (Attribute Base ev) -> List (Node ev) -> Node ev
base = El _

export %inline
blockquote : List (Attribute Blockquote ev) -> List (Node ev) -> Node ev
blockquote = El _

export %inline
body : List (Attribute Tag.Body ev) -> List (Node ev) -> Node ev
body = El _

export %inline
br : List (Attribute Br ev) -> List (Node ev) -> Node ev
br = El _

export %inline
button : List (Attribute Tag.Button ev) -> List (Node ev) -> Node ev
button = El _

export %inline
canvas : List (Attribute Canvas ev) -> List (Node ev) -> Node ev
canvas = El _

export %inline
caption : List (Attribute Caption ev) -> List (Node ev) -> Node ev
caption = El _

export %inline
col : List (Attribute Col ev) -> List (Node ev) -> Node ev
col = El _

export %inline
colgroup : List (Attribute Colgroup ev) -> List (Node ev) -> Node ev
colgroup = El _

export %inline
data_ : List (Attribute Data ev) -> List (Node ev) -> Node ev
data_ = El _

export %inline
datalist : List (Attribute Datalist ev) -> List (Node ev) -> Node ev
datalist = El _

export %inline
del : List (Attribute Del ev) -> List (Node ev) -> Node ev
del = El _

export %inline
details : List (Attribute Details ev) -> List (Node ev) -> Node ev
details = El _

export %inline
dialog : List (Attribute Dialog ev) -> List (Node ev) -> Node ev
dialog = El _

export %inline
div : List (Attribute Div ev) -> List (Node ev) -> Node ev
div = El _

export %inline
dl : List (Attribute Dl ev) -> List (Node ev) -> Node ev
dl = El _

export %inline
embed : List (Attribute Embed ev) -> List (Node ev) -> Node ev
embed = El _

export %inline
fieldset : List (Attribute FieldSet ev) -> List (Node ev) -> Node ev
fieldset = El _

export %inline
footer : List (Attribute Footer ev) -> List (Node ev) -> Node ev
footer = El _

export %inline
form : List (Attribute Form ev) -> List (Node ev) -> Node ev
form = El _

export %inline
h1 : List (Attribute H1 ev) -> List (Node ev) -> Node ev
h1 = El _

export %inline
h2 : List (Attribute H2 ev) -> List (Node ev) -> Node ev
h2 = El _

export %inline
h3 : List (Attribute H3 ev) -> List (Node ev) -> Node ev
h3 = El _

export %inline
h4 : List (Attribute H4 ev) -> List (Node ev) -> Node ev
h4 = El _

export %inline
h5 : List (Attribute H5 ev) -> List (Node ev) -> Node ev
h5 = El _

export %inline
h6 : List (Attribute H6 ev) -> List (Node ev) -> Node ev
h6 = El _

export %inline
header : List (Attribute Header ev) -> List (Node ev) -> Node ev
header = El _

export %inline
hr : List (Attribute HR ev) -> List (Node ev) -> Node ev
hr = El _

export %inline
html : List (Attribute Html ev) -> List (Node ev) -> Node ev
html = El _

export %inline
iframe : List (Attribute IFrame ev) -> List (Node ev) -> Node ev
iframe = El _

export %inline
img : List (Attribute Img ev) -> List (Node ev) -> Node ev
img = El _

export %inline
input : List (Attribute Tag.Input ev) -> List (Node ev) -> Node ev
input = El _

export %inline
ins : List (Attribute Ins ev) -> List (Node ev) -> Node ev
ins = El _

export %inline
label : List (Attribute Label ev) -> List (Node ev) -> Node ev
label = El _

export %inline
legend : List (Attribute Legend ev) -> List (Node ev) -> Node ev
legend = El _

export %inline
li : List (Attribute Li ev) -> List (Node ev) -> Node ev
li = El _

export %inline
link : List (Attribute Link ev) -> List (Node ev) -> Node ev
link = El _

export %inline
map : List (Attribute Tag.Map ev) -> List (Node ev) -> Node ev
map = El _

export %inline
menu : List (Attribute Menu ev) -> List (Node ev) -> Node ev
menu = El _

export %inline
meta : List (Attribute Meta ev) -> List (Node ev) -> Node ev
meta = El _

export %inline
meter : List (Attribute Meter ev) -> List (Node ev) -> Node ev
meter = El _

export %inline
object : List (Attribute Tag.Object ev) -> List (Node ev) -> Node ev
object = El _

export %inline
ol : List (Attribute Ol ev) -> List (Node ev) -> Node ev
ol = El _

export %inline
optgroup : List (Attribute OptGroup ev) -> List (Node ev) -> Node ev
optgroup = El _

export %inline
option : List (Attribute Option ev) -> List (Node ev) -> Node ev
option = El _

export %inline
output : List (Attribute Output ev) -> List (Node ev) -> Node ev
output = El _

export %inline
p : List (Attribute P ev) -> List (Node ev) -> Node ev
p = El _

export %inline
param : List (Attribute Param ev) -> List (Node ev) -> Node ev
param = El _

export %inline
picture : List (Attribute Picture ev) -> List (Node ev) -> Node ev
picture = El _

export %inline
pre : List (Attribute Pre ev) -> List (Node ev) -> Node ev
pre = El _

export %inline
progress : List (Attribute Progress ev) -> List (Node ev) -> Node ev
progress = El _

export %inline
q : List (Attribute Q ev) -> List (Node ev) -> Node ev
q = El _

export %inline
script : List (Attribute Script ev) -> List (Node ev) -> Node ev
script = El _

export %inline
section : List (Attribute Section ev) -> List (Node ev) -> Node ev
section = El _

export %inline
select : List (Attribute Select ev) -> List (Node ev) -> Node ev
select = El _

export %inline
slot : List (Attribute Slot ev) -> List (Node ev) -> Node ev
slot = El _

export %inline
source : List (Attribute Source ev) -> List (Node ev) -> Node ev
source = El _

export %inline
span : List (Attribute Span ev) -> List (Node ev) -> Node ev
span = El _

export %inline
style : List (Attribute Style ev) -> List (Node ev) -> Node ev
style = El _

export %inline
table : List (Attribute Table ev) -> List (Node ev) -> Node ev
table = El _

export %inline
tbody : List (Attribute Tbody ev) -> List (Node ev) -> Node ev
tbody = El _

export %inline
td : List (Attribute Td ev) -> List (Node ev) -> Node ev
td = El _

export %inline
template : List (Attribute Template ev) -> List (Node ev) -> Node ev
template = El _

export %inline
textarea : List (Attribute TextArea ev) -> List (Node ev) -> Node ev
textarea = El _

export %inline
tfoot : List (Attribute Tfoot ev) -> List (Node ev) -> Node ev
tfoot = El _

export %inline
th : List (Attribute Th ev) -> List (Node ev) -> Node ev
th = El _

export %inline
thead : List (Attribute Thead ev) -> List (Node ev) -> Node ev
thead = El _

export %inline
time : List (Attribute Tag.Time ev) -> List (Node ev) -> Node ev
time = El _

export %inline
title : List (Attribute Title ev) -> List (Node ev) -> Node ev
title = El _

export %inline
tr : List (Attribute Tr ev) -> List (Node ev) -> Node ev
tr = El _

export %inline
track : List (Attribute Track ev) -> List (Node ev) -> Node ev
track = El _

export %inline
ul : List (Attribute Ul ev) -> List (Node ev) -> Node ev
ul = El _

export %inline
video : List (Attribute Video ev) -> List (Node ev) -> Node ev
video = El _

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

attrs : List (Attribute t ev) -> String
attrs as = let s = displayAttributes as in if null s then "" else " " ++ s

export
render : Node ev -> String
render n = case n of
  Raw x             => x
  Text x            => escape x
  El {tag} _ as ns  => "<\{tag}\{attrs as}>\{go [<] ns}</\{tag}>"
  Empty             => ""

  where
    go : SnocList String -> List (Node ev) -> String
    go ss (n :: ns) = go (ss :< render n) ns
    go ss []        = concat $ ss <>> []

export
renderMany : List (Node ev) -> String
renderMany = fastConcat . map render
