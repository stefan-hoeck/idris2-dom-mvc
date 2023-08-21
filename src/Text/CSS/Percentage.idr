module Text.CSS.Percentage

import Data.Refined
import Derive.Prelude
import Derive.Refined

%language ElabReflection
%default total

public export
IsPercentage : Double -> Bool
IsPercentage x = 0 <= x && x <= 100

||| A floating point percentage value in the the
||| range [0,100].
public export
record Percentage where
  constructor MkPercentage
  value : Double
  {auto 0 prf : Holds IsPercentage value}

%runElab derive "Percentage" [Show,Eq,Ord,RefinedDouble]

export
Interpolation Percentage where
  interpolate (MkPercentage v) = show v ++ "%"

||| Convenience function for creating percentages with little
||| syntactic overhead.
|||
||| ```idris example
||| perc 12
||| ```
export %inline
perc :
     {auto _ : Cast Percentage a}
  -> (v : Double)
  -> {auto 0 prf : Holds IsPercentage v}
  -> a
perc v = cast $ MkPercentage v
