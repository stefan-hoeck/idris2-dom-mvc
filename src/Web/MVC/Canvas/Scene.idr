module Web.MVC.Canvas.Scene

import Control.Monad.Either.Extra
import JS
import Web.MVC.Canvas.Shape
import Web.MVC.Canvas.Style
import Web.MVC.Canvas.Transformation
import Web.Html

%default total

--------------------------------------------------------------------------------
--          Text Metrics
--------------------------------------------------------------------------------

%foreign "browser:lambda:(x,a)=>x.measureText(a)"
prim__measure : CanvasRenderingContext2D -> String -> PrimIO TextMetrics

export
%foreign "browser:lambda:x=>x.actualBoundingBoxAscent"
actualBoundingBoxAscent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.actualBoundingBoxDescent"
actualBoundingBoxDescent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.actualBoundingBoxLeft"
actualBoundingBoxLeft : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.actualBoundingBoxRight"
actualBoundingBoxRight : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.alphabeticBaseline"
alphabeticBaseline : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.emHeightAscent"
emHeightAscent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.emHeightDescent"
emHeightDescent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.fontBoundingBoxAscent"
fontBoundingBoxAscent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.fontBoundingBoxDescent"
fontBoundingBoxDescent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.hangingBaseline"
hangingBaseline : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.ideographicBaseline"
ideographicBaseline : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.width"
width : TextMetrics -> Double

%foreign "browser:lambda:(c,f,s)=>{f1 = c.font; c.font = f; res = c.measureText(s); c.font = f1; return res}"
prim__measureText : CanvasRenderingContext2D -> (font, text : String) -> TextMetrics

--------------------------------------------------------------------------------
--          Scene
--------------------------------------------------------------------------------

public export
data Scene : Type where
  S1 : (fs : List Style) -> (tr : Transformation) -> (shape : Shape) -> Scene
  SM : (fs : List Style) -> (tr : Transformation) -> List Scene -> Scene

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

||| Utility for computing `TextMetrics`.
public export
record TextMeasure where
  [noHints]
  constructor TM
  measure_ : (font, text : String) -> TextMetrics

||| Compute the `TextMetrics` for the given text in the given font.
export %inline
measureText : (m : TextMeasure) => (font,text : String) -> TextMetrics
measureText f t = m.measure_ f t

||| Alternative version of `apply` for those cases where we need to
||| work with text metrics.
export
applyWithMetrics : CanvasRenderingContext2D -> (TextMeasure => Scene) -> JSIO ()
applyWithMetrics cd f = apply cd (f @{TM $ prim__measureText cd})
