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

%foreign "browser:lambda:(w) => window.devicePixelRatio"
prim__devicePixelRatio : PrimIO Double

%foreign "browser:lambda:(c,pxw,pxh,cssw,cssh,w) => {c.style.width = cssw + \"px\"; c.style.height = cssh + \"px\"; c.width = pxw; c.height = pxh;}"
prim__setDims : HTMLCanvasElement -> (pxw, pxh, cssw, cssh : Double) -> PrimIO ()

export %inline
devicePixelRatio : HasIO io => io Double
devicePixelRatio = primIO prim__devicePixelRatio

||| Canvas dimensions
public export
record CanvasDims where
  [noHints]
  constructor CD

  ||| Device pixel ratio
  pixelRatio : Double

  ||| Pixel width of canvas
  pxWidth    : Double

  ||| Pixel height of canvas
  pxHeight   : Double

  ||| CSS pixel width of canvas
  cssWidth   : Double

  ||| CSS pixel height of canvas
  cssHeight  : Double

%runElab derive "CanvasDims" [Show,Eq]

compCanvasDims : HTMLCanvasElement -> JSIO CanvasDims
compCanvasDims canvas = do
  r      <- Canvas.devicePixelRatio
  w      <- cast <$> get canvas width
  h      <- cast <$> get canvas height
  pure $ CD r w h (w / r) (h / r)

export
canvasDims : Ref Canvas -> JSIO CanvasDims
canvasDims r = castElementByRef {t = HTMLCanvasElement} r >>= compCanvasDims

export
setCanvasDims : Ref Canvas -> CanvasDims -> JSIO ()
setCanvasDims r (CD _ pxw pxh cssw cssh) = do
  canvas <- castElementByRef {t = HTMLCanvasElement} r
  primIO $ prim__setDims canvas {pxw, pxh, cssw, cssh}

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
  dims   <- compCanvasDims canvas
  apply ctxt $ Rect 0 0 dims.pxWidth dims.pxHeight Clear
  applyWithMetrics ctxt (scene dims)

||| Render a scene in a canvas in the DOM.
export %inline
render : Ref Canvas -> (CanvasDims -> Scene) -> JSIO ()
render ref scene = renderWithMetrics ref scene
