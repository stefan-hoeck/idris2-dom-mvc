module Web.MVC.Canvas.Scene

import Control.Monad.Either.Extra
import JS
import Web.MVC.Canvas.Shape
import Web.MVC.Canvas.Style
import Web.MVC.Canvas.Transformation
import Web.MVC.ElemRef
import Web.Html

%default total

public export
data Scene : Type where
  S1 : (fs : List Style) -> (tr : Transformation) -> (shape : Shape) -> Scene
  SM : (fs : List Style) -> (tr : Transformation) -> List Scene -> Scene

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

mutual
  export
  applyAll : CanvasRenderingContext2D -> List Scene -> JSIO ()
  applyAll ctxt = assert_total $ traverseList_ (apply ctxt)

  export
  apply : CanvasRenderingContext2D -> Scene -> JSIO ()
  apply ctxt (S1 fs tr shape) = do
    save    ctxt
    traverseList_ (apply ctxt) fs
    apply   ctxt tr
    apply   ctxt shape
    restore ctxt

  apply ctxt (SM fs tr xs) = do
    save     ctxt
    traverseList_ (apply ctxt) fs
    apply    ctxt tr
    applyAll ctxt xs
    restore  ctxt
