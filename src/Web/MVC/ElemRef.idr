module Web.MVC.ElemRef

import JS
import Text.CSS
import Text.HTML
import Web.Dom

%default total

namespace Tag
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

--------------------------------------------------------------------------------
--          ElemRef
--------------------------------------------------------------------------------

||| A typed reference to an element or container in the DOM. Elements can
||| either be referenced by their ID string or their CSS class
||| (both of which must be unique), or by holding a value directly.
||| This can be used to access the element in question,
||| for instance by invoking `getElementByRef`.
|||
||| In addition, we provide (pseudo-)element references for
||| `body`, `document`, and `window`.
public export
data ElemRef : Type where
  Id :  {tag   : String}
     -> (0 tpe : HTMLTag tag)
     -> (id    : String)
     -> ElemRef

  Class :  {tag   : String}
        -> (0 tpe : HTMLTag tag)
        -> (class : String)
        -> ElemRef

  Body : ElemRef

  Document : ElemRef

  Window : ElemRef

||| DOM type associacte with an ElemRef
public export
0 ElemType : ElemRef -> Type
ElemType (Id _ _)    = HTMLElement
ElemType (Class _ _) = HTMLElement
ElemType Body        = HTMLElement
ElemType Document    = Document
ElemType Window      = Window

||| Predicate witnessing that a given `ElemRef` is a reference
||| by ID.
public export
data ById : ElemRef -> Type where
  IsById : {0 tpe : _} -> {0 id : _} -> ById (Id tpe id)

||| Predicate witnessing that a given `ElemRef` is a reference
||| by Class.
public export
data ByClass : ElemRef -> Type where
  IsByClass : {0 tpe : _} -> {0 id : _} -> ByClass (Class tpe id)

namespace Attribute
  ||| Uses an element ref as an ID attribute
  export
  ref : (r : ElemRef) -> {auto 0 _ : ById r} -> Attribute ev
  ref (Id _ i) = id i

namespace CSS
  ||| Uses an element ref as an ID selector
  export
  idRef : (r : ElemRef) -> {auto 0 _ : ById r} -> List Declaration -> Rule n
  idRef (Id _ i) = id i

  ||| Uses an element ref as a class selector
  export
  classRef : (r : ElemRef) -> {auto 0 _ : ByClass r} -> List Declaration -> Rule n
  classRef (Class _ i) = class i

--------------------------------------------------------------------------------
--          Inserting Nodes
--------------------------------------------------------------------------------

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
strictGetElementById : SafeCast t => (tag,id : String) -> JSIO t
strictGetElementById tag id = do
  Nothing <- castElementById t id | Just t => pure t
  liftJSIO $ throwError $
    Caught "Web.MVC.ElemRef.strictGetElementById: Could not find \{tag} with id \{id}"

||| Tries to retrieve a HTMLElement by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export %inline
strictGetHTMLElementById : (tag,id : String) -> JSIO HTMLElement
strictGetHTMLElementById = strictGetElementById

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
getElementByRef : (r : ElemRef) -> JSIO (ElemType r)
getElementByRef (Id {tag} _ id) = strictGetElementById tag id
getElementByRef (Class _ class) = getElementByClass class
getElementByRef Body            = body
getElementByRef Document        = document
getElementByRef Window          = window

err : String
err = "Web.MVC.ElemRef.castElementByRef"

||| Tries to retrieve an element of the given type by looking
||| up its ID in the DOM. Unlike `getElementById`, this will throw
||| an exception in the `JSIO` monad if the element is not found
||| or can't be safely cast to the desired type.
export
castElementByRef : SafeCast t => ElemRef -> JSIO t
castElementByRef (Id {tag} _ id) = strictGetElementById tag id
castElementByRef (Class _ class) = getElementByClass class
castElementByRef Body            = body >>= tryCast err
castElementByRef Document        = document >>= tryCast err
castElementByRef Window          = window >>= tryCast err
