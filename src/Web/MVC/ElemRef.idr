module Web.MVC.ElemRef

import JS
import Text.CSS
import Text.HTML
import Web.Dom
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
