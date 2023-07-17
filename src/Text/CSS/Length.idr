module Text.CSS.Length

%default total

public export
data Length : Type where
  Pt       : Bits16 -> Length
  Px       : Bits16 -> Length
  Em       : Double -> Length
  Rem      : Double -> Length

export
Interpolation Length where
  interpolate (Pt x)  = show x ++ "pt"
  interpolate (Px x)  = show x ++ "px"
  interpolate (Em x)  = show x ++ "em"
  interpolate (Rem x) = show x ++ "rem"

export %inline
pt : Cast Length a => Bits16 -> a
pt = cast . Pt

export %inline
px : Cast Length a => Bits16 -> a
px = cast . Px

export %inline
em : Cast Length a => Double -> a
em = cast . Em

export %inline
rem : Cast Length a => Double -> a
rem = cast . Rem
