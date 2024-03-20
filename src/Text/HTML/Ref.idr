module Text.HTML.Ref

import Text.HTML.Tag

%default total

||| A typed reference to an element or container in the DOM. Elements can
|||
||| This can be used as a type-safe ID when constructing
||| HTML nodes and their attribute lists.
||| In addition, we provide (pseudo-)element references for
||| `body`, `document`, and `window`.
public export
data Ref : {0 k : Type} -> (t : k) -> Type where
  Id :  {tag   : String}
     -> {0 tpe : HTMLTag tag}
     -> (id    : String)
     -> Ref tpe

  Elem     : String -> Ref Void

  Body     : Ref Void

  Document : Ref Void

  Window   : Ref Void
