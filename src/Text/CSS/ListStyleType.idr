module Text.CSS.ListStyleType

%default total

public export
data ListStyleType : Type where
  None                : ListStyleType
  Disc                : ListStyleType
  Circle              : ListStyleType
  Square              : ListStyleType
  Decimal             : ListStyleType
  LowerAlpha          : ListStyleType
  UpperAlpha          : ListStyleType
  LowerRoman          : ListStyleType
  UpperRoman          : ListStyleType

export
Interpolation ListStyleType where
  interpolate None       = "none"
  interpolate Disc       = "disc"
  interpolate Circle     = "circle"
  interpolate Square     = "square"
  interpolate Decimal    = "decimal"
  interpolate LowerAlpha = "lower-alpha"
  interpolate UpperAlpha = "upper-alpha"
  interpolate LowerRoman = "lower-roman"
  interpolate UpperRoman = "upper-roman"
