module Web.MVC.DOMUpdate

import Data.Either
import Data.Maybe
import Data.String
import Text.CSS
import Text.HTML
import Web.MVC.ElemRef
import Web.MVC.Canvas.Scene

%default total

public export
data DOMUpdate : Type -> Type where
  Children    : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Replace     : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Append      : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Prepend     : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  After       : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Before      : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Attr        : ElemRef -> Attribute e -> DOMUpdate e
  Value       : (r : ElemRef) -> Value r => String -> DOMUpdate e
  ValidityMsg : (r : ElemRef) -> SetValidity r => String -> DOMUpdate e
  Remove      : ElemRef -> DOMUpdate e
  Render      : ElemRef -> Scene -> DOMUpdate e
  NoAction    : DOMUpdate e

export %inline
child : ElemRef -> Node e -> DOMUpdate e
child ref = Children ref . (::[])

export %inline
text : ElemRef -> String -> DOMUpdate e
text ref = child ref . Text

export %inline
style' : ElemRef -> String -> DOMUpdate e
style' ref = child ref . Raw

export
style : ElemRef -> List (Rule 1) -> DOMUpdate e
style ref rules =
  let str := fastUnlines $ map interpolate rules
   in style' ref str

export %inline
show : Show t => ElemRef -> t -> DOMUpdate e
show ref = text ref . show

export %inline
replace : ElemRef -> Node e -> DOMUpdate e
replace ref = Replace ref . (::[])

export %inline
append : ElemRef -> Node e -> DOMUpdate e
append ref = Append ref . (::[])

export %inline
prepend : ElemRef -> Node e -> DOMUpdate e
prepend ref = Prepend ref . (::[])

export %inline
after : ElemRef -> Node e -> DOMUpdate e
after ref = After ref . (::[])

export %inline
before : ElemRef -> Node e -> DOMUpdate e
before ref = Before ref . (::[])

export %inline
validate : (r : ElemRef) -> SetValidity r => Either String t -> DOMUpdate e
validate ref (Left s)  = ValidityMsg ref s
validate ref (Right s) = ValidityMsg ref ""

export %inline
disabled : (r : ElemRef) -> Bool -> DOMUpdate e
disabled r = Attr r . disabled

export %inline
disabledE : (r : ElemRef) -> Either t b -> DOMUpdate e
disabledE r = disabled r . isLeft

export %inline
disabledM : (r : ElemRef) -> Maybe t -> DOMUpdate e
disabledM r = disabled r . isNothing

export
updateIf : Bool -> Lazy (DOMUpdate e) -> DOMUpdate e
updateIf True  u = u
updateIf False _ = NoAction
