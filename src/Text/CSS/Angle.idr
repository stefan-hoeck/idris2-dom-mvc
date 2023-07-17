module Text.CSS.Angle

%default total

public export
data Angle : Type where
  Deg  : Double -> Angle
  Rad  : Double -> Angle
  Grad : Double -> Angle
  Turn : Double -> Angle

export
Interpolation Angle where
  interpolate (Deg x)  = show x ++ "deg"
  interpolate (Rad x)  = show x ++ "rad"
  interpolate (Grad x) = show x ++ "grad"
  interpolate (Turn x) = show x ++ "turn"

export %inline
deg : Cast Angle a => Double -> a
deg = cast . Deg

export %inline
rad : Cast Angle a => Double -> a
rad = cast . Rad

export %inline
grad : Cast Angle a => Double -> a
grad = cast . Grad

export %inline
turn : Cast Angle a => Double -> a
turn = cast . Turn
