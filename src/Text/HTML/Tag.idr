module Text.HTML.Tag

%default total

||| HTML Element Tags linking tag names with an enumeration.
|||
||| Some deprecated tags have been left out, some others might
||| still be missing.
public export
data HTMLTag : (tag : String) -> Type where
  A          : HTMLTag "a"
  Address    : HTMLTag "address"
  Area       : HTMLTag "area"
  Article    : HTMLTag "article"
  Aside      : HTMLTag "aside"
  Audio      : HTMLTag "audio"
  Base       : HTMLTag "base"
  Blockquote : HTMLTag "blockquote"
  Body       : HTMLTag "body"
  Br         : HTMLTag "br"
  Button     : HTMLTag "button"
  Canvas     : HTMLTag "canvas"
  Caption    : HTMLTag "caption"
  Col        : HTMLTag "col"
  Colgroup   : HTMLTag "colgroup"
  Data       : HTMLTag "data"
  Datalist   : HTMLTag "datalist"
  Del        : HTMLTag "del"
  Details    : HTMLTag "details"
  Dialog     : HTMLTag "dialog"
  Div        : HTMLTag "div"
  Dl         : HTMLTag "dl"
  Embed      : HTMLTag "embed"
  FieldSet   : HTMLTag "fieldset"
  Footer     : HTMLTag "footer"
  Form       : HTMLTag "form"
  H1         : HTMLTag "h1"
  H2         : HTMLTag "h2"
  H3         : HTMLTag "h3"
  H4         : HTMLTag "h4"
  H5         : HTMLTag "h5"
  H6         : HTMLTag "h6"
  HR         : HTMLTag "hr"
  Header     : HTMLTag "header"
  Html       : HTMLTag "html"
  IFrame     : HTMLTag "iframe"
  Img        : HTMLTag "img"
  Input      : HTMLTag "input"
  Ins        : HTMLTag "ins"
  Label      : HTMLTag "label"
  Legend     : HTMLTag "legend"
  Li         : HTMLTag "li"
  Link       : HTMLTag "link"
  Map        : HTMLTag "map"
  Menu       : HTMLTag "menu"
  Meta       : HTMLTag "meta"
  Meter      : HTMLTag "meter"
  Object     : HTMLTag "object"
  Ol         : HTMLTag "ol"
  OptGroup   : HTMLTag "optgroup"
  Option     : HTMLTag "option"
  Output     : HTMLTag "output"
  P          : HTMLTag "p"
  Param      : HTMLTag "param"
  Picture    : HTMLTag "picture"
  Pre        : HTMLTag "pre"
  Progress   : HTMLTag "progress"
  Q          : HTMLTag "q"
  Script     : HTMLTag "script"
  Section    : HTMLTag "section"
  Select     : HTMLTag "select"
  Slot       : HTMLTag "slot"
  Source     : HTMLTag "source"
  Span       : HTMLTag "span"
  Style      : HTMLTag "style"
  Svg        : HTMLTag "svg"
  Table      : HTMLTag "table"
  Tbody      : HTMLTag "tbody"
  Td         : HTMLTag "td"
  Template   : HTMLTag "template"
  TextArea   : HTMLTag "textarea"
  Tfoot      : HTMLTag "tfoot"
  Th         : HTMLTag "th"
  Thead      : HTMLTag "thead"
  Time       : HTMLTag "time"
  Title      : HTMLTag "title"
  Tr         : HTMLTag "tr"
  Track      : HTMLTag "track"
  Ul         : HTMLTag "ul"
  Video      : HTMLTag "video"

||| Proof that we can set a custom validity message to
||| a HTML object with this tag.
public export
data ValidityTag : (t : HTMLTag s) -> Type where
  SVButton   : ValidityTag Button
  SVFieldSet : ValidityTag FieldSet
  SVInput    : ValidityTag Input
  SVObject   : ValidityTag Object
  SVOutput   : ValidityTag Output
  SVSelect   : ValidityTag Select
  SVTextArea : ValidityTag TextArea

||| Proof that we can set a string value to
||| a HTML object with this tag.
public export
data ValueTag : (t : HTMLTag s) -> Type where
  VButton   : ValueTag Button
  VData     : ValueTag Data
  VInput    : ValueTag Input
  VOption   : ValueTag Option
  VOutput   : ValueTag Output
  VParam    : ValueTag Param
  VSelect   : ValueTag Select
  VTextArea : ValueTag TextArea
