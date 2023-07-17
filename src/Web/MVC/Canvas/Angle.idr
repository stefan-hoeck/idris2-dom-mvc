module Web.MVC.Canvas.Angle

%default total

--------------------------------------------------------------------------------
--          Angle
--------------------------------------------------------------------------------

public export
data Angle : Type where
  Radians : Double -> Angle
  Degree : Double -> Angle

export %inline
rad : Double -> Angle
rad = Radians

export %inline
deg : Double -> Angle
deg = Degree

export
toRadians : Angle -> Double
toRadians (Radians x) = x
toRadians (Degree x)  = (x / 180) * pi

export
toDegree : Angle -> Double
toDegree (Radians x) = (x / pi) * 180
toDegree (Degree x)  = x

export
Show Angle where
  show = show . toDegree
