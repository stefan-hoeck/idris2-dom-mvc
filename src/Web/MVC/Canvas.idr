module Web.MVC.Canvas

import JS
import Web.Html
import Web.MVC.ElemRef

import public Web.MVC.Canvas.Angle
import public Web.MVC.Canvas.Scene
import public Web.MVC.Canvas.Shape
import public Web.MVC.Canvas.Style
import public Web.MVC.Canvas.Transformation

%default total

export
context2D : HTMLCanvasElement -> JSIO CanvasRenderingContext2D
context2D canvas = do
  m      <- getContext canvas "2d"
  case m >>= project CanvasRenderingContext2D of
    Just c  => pure c
    Nothing => throwError $ Caught "Web.MVC.Canvas.context2d: No rendering context for canvas"

export
render : ElemRef -> Scene -> JSIO ()
render ref scene = do
  canvas <- castElementByRef {t = HTMLCanvasElement} ref
  ctxt   <- context2D canvas
  w      <- get canvas width
  h      <- get canvas height
  apply ctxt $ Rect 0 0 (cast w) (cast h) Clear
  apply ctxt scene
