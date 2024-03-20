module Text.HTML.DomID

import Data.List.Quantifiers
import Derive.Prelude
import Text.HTML

%default total
%language ElabReflection

||| Utility data type for accessing elements in the DOM.
|||
||| This will improve type safety and give clearer semantics to a string
||| that is to be used as an identifier in the DOM.
public export
record DomID where
  constructor D
  value : String

%runElab derive "DomID" [Show,Eq,Ord,FromString]

||| Alias for `cast` with better type inference.
export %inline
toID : Cast t DomID => t -> DomID
toID = cast

||| Specialized version of `withId` (which sets the `id` attribute of a
||| node) for working with `DomID`s.
export %inline
withDomID : Cast t DomID => t -> Node e -> Node e
withDomID = withId . value . toID

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

export
Interpolation DomID where interpolate = value

export
Semigroup DomID where
  D x <+> D y = D $ x ++ "-" ++ y

export %inline
Cast Nat DomID where cast = D . show

export
Cast x DomID => Cast (SnocList x) DomID where
  cast [<]       = D "lin"
  cast (sx :< x) = cast sx <+> cast x

export
Cast x DomID => Cast (List x) DomID where
  cast []      = D "nil"
  cast (x::xs) = cast x <+> cast xs

export
(prf : All (\x => Cast (f x) DomID) ks) => Cast (List.Quantifiers.All.All f ks) DomID where
  cast []              = "nil"
  cast @{_::_} (x::xs) = cast x <+> cast xs

--------------------------------------------------------------------------------
-- Accessing DOM Elements
--------------------------------------------------------------------------------

export %inline
tagRef : {s : _} -> (0 tag : HTMLTag s) -> Cast t DomID => t -> Ref tag
tagRef _ v = Id (value $ cast v)

||| Identifier for accessing a `<button>` element
export %inline
btnRef : Cast t DomID => t -> Ref Tag.Button
btnRef = tagRef _

||| Identifier for accessing a `<div>` element
export %inline
divRef : Cast t DomID => t -> Ref Tag.Div
divRef = tagRef _

||| Identifier for accessing an `<input>` element
export %inline
inpRef : Cast t DomID => t -> Ref Tag.Input
inpRef = tagRef _

||| Identifier for accessing a `<canvas>` element
export %inline
canvasRef : Cast t DomID => t -> Ref Tag.Canvas
canvasRef = tagRef _

||| `id` attribute for a DOM element
export %inline
ref : {s : _} -> {0 tag : HTMLTag s} -> Cast t DomID => t -> Attribute tag e
ref = Id . tagRef tag
