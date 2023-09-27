module Web.MVC.Canvas.Scene

import Control.Monad.Either.Extra
import JS
import Web.MVC.Canvas.Shape
import Web.MVC.Canvas.Style
import Web.MVC.Canvas.Transformation
import Web.Html

%default total

%hide Types.TextMetrics

export
data TextMetrics : Type where [external]

%foreign "browser:lambda:(x,a)=>x.measureText(a)"
prim__measure : CanvasRenderingContext2D -> String -> PrimIO TextMetrics

public export
data Scene : Type where
  S1 : (fs : List Style) -> (tr : Transformation) -> (shape : Shape) -> Scene
  SM : (fs : List Style) -> (tr : Transformation) -> List Scene -> Scene
  ST : (txt, font  : String) -> (TextMetrics -> Scene) -> Scene

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

export
applyAll : CanvasRenderingContext2D -> List Scene -> JSIO ()

export
apply : CanvasRenderingContext2D -> Scene -> JSIO ()

applyAll ctxt = assert_total $ traverseList_ (apply ctxt)

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

apply ctxt (ST txt fnt f) = do
  save ctxt
  font ctxt .= fnt
  m <- liftIO $ fromPrim (prim__measure ctxt txt)
  restore ctxt
  apply ctxt (f m)
