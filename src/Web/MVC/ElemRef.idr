module Web.MVC.ElemRef

import JS
import Text.CSS
import Text.HTML
import Web.Dom
import Web.Html
import public Text.HTML.Tag

%default total

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
  Id :  {tag : String}
     -> (tpe : HTMLTag tag)
     -> (id  : String)
     -> ElemRef

  Class :  {tag   : String}
        -> (tpe   : HTMLTag tag)
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

--------------------------------------------------------------------------------
--          Predicates
--------------------------------------------------------------------------------

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

public export
data SetValidityTag : (t : HTMLTag s) -> Type where
  SVButton   : SetValidityTag Button
  SVFieldSet : SetValidityTag FieldSet
  SVInput    : SetValidityTag Input
  SVObject   : SetValidityTag Object
  SVOutput   : SetValidityTag Output
  SVSelect   : SetValidityTag Select
  SVTextArea : SetValidityTag TextArea

public export
data SetValidity : (r : ElemRef) -> Type where
  SV : {auto prf : SetValidityTag t} -> SetValidity (Id t i)

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

public export
data Value : (r : ElemRef) -> Type where
  V : {auto prf : ValueTag t} -> Value (Id t i)

--------------------------------------------------------------------------------
--          Attributes and Rules
--------------------------------------------------------------------------------

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
--          Accessing and Updating Nodes
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
    Caught "Web.MVC.Reactimate.strictGetElementById: Could not find \{tag} with id \{id}"

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
err = "Web.MVC.Reactimate.castElementByRef"

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

setVM : ElemRef -> SetValidityTag t -> String -> JSIO ()
setVM r SVButton s   = castElementByRef r >>= \x => HTMLButtonElement.setCustomValidity x s
setVM r SVFieldSet s = castElementByRef r >>= \x => HTMLFieldSetElement.setCustomValidity x s
setVM r SVInput s    = castElementByRef r >>= \x => HTMLInputElement.setCustomValidity x s
setVM r SVObject s   = castElementByRef r >>= \x => HTMLObjectElement.setCustomValidity x s
setVM r SVOutput s   = castElementByRef r >>= \x => HTMLOutputElement.setCustomValidity x s
setVM r SVSelect s   = castElementByRef r >>= \x => HTMLSelectElement.setCustomValidity x s
setVM r SVTextArea s = castElementByRef r >>= \x => HTMLTextAreaElement.setCustomValidity x s

setVal : ElemRef -> ValueTag t -> String -> JSIO ()
setVal r VButton s   = castElementByRef r >>= (HTMLButtonElement.value =. s)
setVal r VData s     = castElementByRef r >>= (HTMLDataElement.value =. s)
setVal r VInput s    = castElementByRef r >>= (HTMLInputElement.value =. s)
setVal r VOption s   = castElementByRef r >>= (HTMLOptionElement.value =. s)
setVal r VOutput s   = castElementByRef r >>= (HTMLOutputElement.value =. s)
setVal r VParam s    = castElementByRef r >>= (HTMLParamElement.value =. s)
setVal r VSelect s   = castElementByRef r >>= (HTMLSelectElement.value =. s)
setVal r VTextArea s = castElementByRef r >>= (HTMLTextAreaElement.value =. s)

export
setValidityMessage : (r : ElemRef) -> SetValidity r => String -> JSIO ()
setValidityMessage (Id t i) @{SV @{p}} = setVM (Id t i) p

export
setValue : (r : ElemRef) -> Value r => String -> JSIO ()
setValue (Id t i) @{V @{p}} = setVal (Id t i) p

--------------------------------------------------------------------------------
--          DOM Updates
--------------------------------------------------------------------------------

nodeList : DocumentFragment -> List (HSum [Node,String])
nodeList df = [inject $ df :> Node]

||| Replaces all children of the given node with a new document fragment.
export %inline
replaceChildren : Element -> DocumentFragment -> JSIO ()
replaceChildren elem = replaceChildren elem . nodeList

||| Appends the given document fragment to a DOM element's children
export %inline
appendDF : Element -> DocumentFragment -> JSIO ()
appendDF elem = append elem . nodeList

||| Prepends the given document fragment to a DOM element's children
export %inline
prependDF : Element -> DocumentFragment -> JSIO ()
prependDF elem = prepend elem . nodeList

||| Inserts the given document fragment after a DOM element.
export %inline
afterDF : Element -> DocumentFragment -> JSIO ()
afterDF elem = after elem . nodeList

||| Inserts the given document fragment before a DOM element.
export %inline
beforeDF : Element -> DocumentFragment -> JSIO ()
beforeDF elem = before elem . nodeList

||| Inserts the given document fragment before a DOM element.
export %inline
replaceDF : Element -> DocumentFragment -> JSIO ()
replaceDF elem = replaceWith elem . nodeList
