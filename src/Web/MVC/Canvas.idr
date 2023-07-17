module Web.MVC.Canvas

import Data.List
import JS
import Web.Html
import Web.MVC

import public Web.MVC.Canvas.Angle
import public Web.MVC.Canvas.Scene
import public Web.MVC.Canvas.Shape
import public Web.MVC.Canvas.Style
import public Web.MVC.Canvas.Transformation

%default total

public export
record Canvas where
  constructor MkCanvas
  ref           : ElemRef
  width, height : Double
  scene         : Scene

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

export
context2D : ElemRef -> JSIO CanvasRenderingContext2D
context2D ref = do
  canvas <- castElementByRef {t = HTMLCanvasElement} ref
  m      <- getContext canvas "2d"
  case m >>= project CanvasRenderingContext2D of
    Just c  => pure c
    Nothing => throwError $ Caught "Web.MVC.Canvas.context2d: No rendering context for canvas"

export
render : Canvas -> JSIO ()
render (MkCanvas ref w h scene) = do
  ctxt <- context2D ref
  apply ctxt $ Rect 0 0 w h Clear
  apply ctxt scene
