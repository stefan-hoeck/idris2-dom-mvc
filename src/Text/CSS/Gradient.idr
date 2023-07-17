module Text.CSS.Gradient

import Data.List
import Data.Nat
import Data.String
import Text.CSS.Angle
import Text.CSS.Color
import Text.CSS.Length
import Text.CSS.Percentage

%default total

--------------------------------------------------------------------------------
--          Side or Corner
--------------------------------------------------------------------------------

public export
data SideOrCorner : Type where
  Left        : SideOrCorner
  Right       : SideOrCorner
  Top         : SideOrCorner
  Bottom      : SideOrCorner
  TopLeft     : SideOrCorner
  TopRight    : SideOrCorner
  BottomLeft  : SideOrCorner
  BottomRight : SideOrCorner

export
Interpolation SideOrCorner where
  interpolate Left        = "left"
  interpolate Right       = "right"
  interpolate Top         = "top"
  interpolate Bottom      = "bottom"
  interpolate TopLeft     = "top left"
  interpolate TopRight    = "top right"
  interpolate BottomLeft  = "bottom left"
  interpolate BottomRight = "bottom right"

--------------------------------------------------------------------------------
--          Linear Direction
--------------------------------------------------------------------------------

public export
data LinearDirection : Type where
  Deflt : LinearDirection
  Angle : Angle -> LinearDirection
  To    : SideOrCorner -> LinearDirection

public export %inline
Cast Angle LinearDirection where
  cast = Angle

export
Interpolation LinearDirection where
  interpolate Deflt     = ""
  interpolate (Angle x) = "\{x}"
  interpolate (To x)    = "to \{x}"

--------------------------------------------------------------------------------
--          Length or Percentage
--------------------------------------------------------------------------------

public export
data LengthOrPercentage : Type where
  L : Length -> LengthOrPercentage
  P : Percentage -> LengthOrPercentage

export %inline
Cast Length LengthOrPercentage where cast = L

export %inline
Cast Percentage LengthOrPercentage where cast = P

export
Interpolation LengthOrPercentage where
  interpolate (L x) = interpolate x
  interpolate (P x) = interpolate x

--------------------------------------------------------------------------------
--          Color Stop List
--------------------------------------------------------------------------------

public export
data CSLState = Empty | Stop | Hint

public export
data ColorStopListElem : (st : CSLState) -> Type where
  C :
       Color
    -> (ps : List LengthOrPercentage)
    -> {auto 0 prf : LTE (length ps) 2}
    -> ColorStopListElem Stop
  H : LengthOrPercentage -> ColorStopListElem Hint

export %inline
col : Color -> ColorStopListElem Stop
col c = C c []

export
Interpolation (ColorStopListElem st) where
  interpolate (C c ps)  = unwords $ interpolate c :: map interpolate ps
  interpolate (H x)     = "\{x}"

public export
data Match : CSLState -> CSLState -> Type where
  MatchStop : Match Stop s
  MatchHint : Match Hint Stop

public export
data ColorStopList : (st : CSLState) -> Type where
  Nil  : ColorStopList Empty
  (::) :
       (h : ColorStopListElem sh)
    -> (t : ColorStopList st)
    -> {auto 0 prf : Match sh st}
    -> ColorStopList sh

export
Interpolation (ColorStopList st) where
  interpolate [v]    = "\{v}"
  interpolate (h::t) = "\{h}, \{t}"
  interpolate []     = ""

--------------------------------------------------------------------------------
--          Gradient
--------------------------------------------------------------------------------

public export
data Gradient : Type where
  Linear :
       (dir    : LinearDirection)
    -> (colors : ColorStopList Stop)
    -> Gradient

export
Interpolation Gradient where
  interpolate (Linear Deflt cs) = "linear-gradient(\{cs})"
  interpolate (Linear x cs)     = "linear-gradient(\{x}, \{cs})"
