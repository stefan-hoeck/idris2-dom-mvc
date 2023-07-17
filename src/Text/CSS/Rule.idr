module Text.CSS.Rule

import Data.String
import Text.CSS.Declaration
import Text.CSS.Selector
import Web.Dom

%default total

public export
data Rule : (n : Nat) -> Type where
  Sel :
       (selectors : List Selector)
    -> (decls     : List Declaration)
    -> Rule n

  Media :
       (query : String)
    -> (rules : List $ Rule 0)
    -> Rule 1

export %inline
sel : Selector -> List Declaration -> Rule n
sel s = Sel [s]

export %inline
class : String -> List Declaration -> Rule n
class s = sel (class s)

export
classes : List String -> List Declaration -> Rule n
classes = sel . classes

export %inline
elem : {str : _} -> (0 tpe : ElementType str t) -> List Declaration -> Rule n
elem v = sel $ elem v

export %inline
id : String -> List Declaration -> Rule n
id = sel . id

export %inline
star : List Declaration -> Rule n
star = sel Star

export
Interpolation (Rule n) where
  interpolate (Sel s ds)    =
    let dss := fastConcat $ map interpolate ds
        ss  := fastConcat . intersperse ", " $ map interpolate s
     in "\{ss}{\{dss}}"
  interpolate (Media q rs)  = "@media (\{q}){\{unlines $ map interpolate rs}}"
