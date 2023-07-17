||| Types and utilities for laying out components in a grid.
module Text.CSS.Grid

import Data.List
import Text.CSS.Length
import Text.CSS.Percentage

--------------------------------------------------------------------------------
--          Flex Values
--------------------------------------------------------------------------------

public export
record Flex where
  constructor MkFlex
  value : Bits16

export %inline
fr : Cast Flex a => Bits16 -> a
fr = cast . MkFlex

export
Interpolation Flex where
  interpolate f = "\{show f.value}fr"

--------------------------------------------------------------------------------
--          MinMax Values
--------------------------------------------------------------------------------

public export
data MinMaxValue : Type where
  Auto       : MinMaxValue
  MML        : Length     -> MinMaxValue
  MMP        : Percentage -> MinMaxValue
  MMF        : Flex       -> MinMaxValue
  MinContent : MinMaxValue
  MaxContent : MinMaxValue

export %inline
Cast Length MinMaxValue where
  cast = MML

export %inline
Cast Percentage MinMaxValue where
  cast = MMP

export %inline
Cast Flex MinMaxValue where
  cast = MMF

export
Interpolation MinMaxValue where
  interpolate Auto       = "auto"
  interpolate MinContent = "min-content"
  interpolate MaxContent = "max-content"
  interpolate (MML x)    = interpolate x
  interpolate (MMP x)    = interpolate x
  interpolate (MMF x)    = interpolate x

--------------------------------------------------------------------------------
--          GridValue
--------------------------------------------------------------------------------

namespace GridValue
  public export
  data GridValue : Type where
    GL         : Length -> GridValue
    GP         : Percentage -> GridValue
    GF         : Flex -> GridValue
    MinMax     : (min,max : MinMaxValue) -> GridValue
    MaxContent : GridValue
    MinContent : GridValue

export
Interpolation GridValue where
  interpolate (GL x)           = interpolate x
  interpolate (GP x)           = interpolate x
  interpolate (GF x)           = interpolate x
  interpolate (MinMax min max) = "minmax(\{min}, \{max})"
  interpolate MaxContent       = "max-content"
  interpolate MinContent       = "min-content"

export
Interpolation (List GridValue) where
  interpolate = fastConcat . intersperse " " . map interpolate

export %inline
Cast Length GridValue where
  cast = GL

export %inline
Cast Percentage GridValue where
  cast = GP

export %inline
Cast Flex GridValue where
  cast = GF

--------------------------------------------------------------------------------
--          GridPosition
--------------------------------------------------------------------------------

public export
data GridPosition : Type where
  At     : Bits32 -> GridPosition
  FromTo : Bits32 -> Bits32 -> GridPosition

export
Interpolation GridPosition where
  interpolate (At x)       = show x
  interpolate (FromTo x y) = "\{show x} / \{show y}"
