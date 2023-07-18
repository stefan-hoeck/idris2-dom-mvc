module Web.MVC.DOMUpdate

import Data.Either
import Data.Maybe
import Data.String
import Text.CSS
import Text.HTML
import Web.MVC.Canvas.Scene

%default total

public export
data DOMUpdate : Type -> Type where
  Children    : Ref t -> (ns : List (Node e)) -> DOMUpdate e
  Replace     : Ref t -> (ns : List (Node e)) -> DOMUpdate e
  Append      : Ref t -> (ns : List (Node e)) -> DOMUpdate e
  Prepend     : Ref t -> (ns : List (Node e)) -> DOMUpdate e
  After       : Ref t -> (ns : List (Node e)) -> DOMUpdate e
  Before      : Ref t -> (ns : List (Node e)) -> DOMUpdate e
  Attr        : Ref t -> Attribute t e -> DOMUpdate e
  Value       : Ref t -> ValueTag t => String -> DOMUpdate e
  ValidityMsg : Ref t -> ValidityTag t => String -> DOMUpdate e
  Remove      : Ref t -> DOMUpdate e
  Render      : Ref Canvas -> Scene -> DOMUpdate e
  NoAction    : DOMUpdate e

export %inline
child : Ref t -> Node e -> DOMUpdate e
child ref = Children ref . (::[])

export %inline
text : Ref t -> String -> DOMUpdate e
text ref = child ref . Text

export %inline
style' : Ref Style -> String -> DOMUpdate e
style' ref = child ref . Raw

export
style : Ref Style -> List (Rule 1) -> DOMUpdate e
style ref rules =
  let str := fastUnlines $ map interpolate rules
   in style' ref str

export %inline
show : Show x => Ref t -> x -> DOMUpdate e
show ref = text ref . show

export %inline
replace : Ref t -> Node e -> DOMUpdate e
replace ref = Replace ref . (::[])

export %inline
append : Ref t -> Node e -> DOMUpdate e
append ref = Append ref . (::[])

export %inline
prepend : Ref t -> Node e -> DOMUpdate e
prepend ref = Prepend ref . (::[])

export %inline
after : Ref t -> Node e -> DOMUpdate e
after ref = After ref . (::[])

export %inline
before : Ref t -> Node e -> DOMUpdate e
before ref = Before ref . (::[])

export %inline
validate : Ref t -> ValidityTag t => Either String x -> DOMUpdate e
validate ref (Left s)  = ValidityMsg ref s
validate ref (Right s) = ValidityMsg ref ""

export %inline
disabled : Ref t -> Bool -> DOMUpdate e
disabled r = Attr r . disabled

export %inline
disabledE : Ref t -> Either x b -> DOMUpdate e
disabledE r = disabled r . isLeft

export %inline
disabledM : Ref t -> Maybe x -> DOMUpdate e
disabledM r = disabled r . isNothing

export
updateIf : Bool -> Lazy (DOMUpdate e) -> DOMUpdate e
updateIf True  u = u
updateIf False _ = NoAction
