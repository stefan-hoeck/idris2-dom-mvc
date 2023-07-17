module Web.MVC.Canvas.Transformation

import JS
import Web.Html
import Web.MVC.Canvas.Angle

--------------------------------------------------------------------------------
--          Transformation
--------------------------------------------------------------------------------

public export
data Transformation : Type where
  Id        : Transformation
  Transform : (a,b,c,d,e,f : Double) -> Transformation

export
scale : (h,w : Double) -> Transformation
scale h w = Transform h 0 0 w 0 0

export
rotate : Angle -> Transformation
rotate phi =
  let r = toRadians phi
      c = cos r
      s = sin r
   in Transform c s (-s) c 0 0

export
translate : (dx,dy : Double) -> Transformation
translate dx dy = Transform 0 0 0 0 dx dy

export
mult : Transformation -> Transformation -> Transformation
mult Id x  = x
mult x  Id = x
mult (Transform a1 b1 c1 d1 e1 f1) (Transform a2 b2 c2 d2 e2 f2) =
  Transform (a1 * a2 + c1 * b2)
            (b1 * a2 + d1 * b2)
            (a1 * c2 + c1 * d2)
            (b1 * c2 + d1 * d2)
            (a1 * e2 + c1 * f2 + e1)
            (b1 * e2 + d1 * f2 + f1)

export %inline
Semigroup Transformation where
  (<+>) = mult

export %inline
Monoid Transformation where
  neutral = Id

--------------------------------------------------------------------------------
--          IO
--------------------------------------------------------------------------------

export
apply : CanvasRenderingContext2D -> Transformation -> JSIO ()
apply ctxt Id                      = pure ()
apply ctxt (Transform a b c d e f) = setTransform ctxt a b c d e f
