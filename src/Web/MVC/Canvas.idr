module Web.MVC.Canvas

import Derive.Prelude
import JS
import Text.HTML.Ref
import Text.HTML.Tag
import Web.Html
import Web.MVC.Util

import public Web.MVC.Canvas.Angle
import public Web.MVC.Canvas.Scene
import public Web.MVC.Canvas.Shape
import public Web.MVC.Canvas.Style
import public Web.MVC.Canvas.Transformation

%default total
%language ElabReflection

||| Canvas dimensions
public export
record CanvasDims where
  [noHints]
  constructor CD
  cwidth  : Double
  cheight : Double

%runElab derive "CanvasDims" [Show,Eq]

export
canvasDims : Ref Canvas -> JSIO CanvasDims
canvasDims r = do
  canvas <- castElementByRef {t = HTMLCanvasElement} r
  w      <- cast <$> get canvas width
  h      <- cast <$> get canvas height
  pure $ CD w h

export
setCanvasDims : Ref Canvas -> CanvasDims -> JSIO ()
setCanvasDims r (CD w h) = do
  canvas <- castElementByRef {t = HTMLCanvasElement} r
  set (width  canvas) (cast w)
  set (height canvas) (cast h)

export
context2D : HTMLCanvasElement -> JSIO CanvasRenderingContext2D
context2D canvas = do
  m      <- getContext canvas "2d"
  case m >>= project CanvasRenderingContext2D of
    Just c  => pure c
    Nothing => throwError $ Caught "Web.MVC.Canvas.context2d: No rendering context for canvas"

||| Render a scene in a canvas in the DOM.
export
renderWithMetrics : Ref Canvas -> (TextMeasure => CanvasDims -> Scene) -> JSIO ()
renderWithMetrics ref scene = do
  canvas <- castElementByRef {t = HTMLCanvasElement} ref
  ctxt   <- context2D canvas
  w      <- cast <$> get canvas width
  h      <- cast <$> get canvas height
  apply ctxt $ Rect 0 0 w h Clear
  applyWithMetrics ctxt (scene (CD w h))

||| Render a scene in a canvas in the DOM.
export %inline
render : Ref Canvas -> (CanvasDims -> Scene) -> JSIO ()
render ref scene = renderWithMetrics ref scene
