||| CSS Rules for the Inc. Buttons Example
module Examples.CSS.Reset

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Text.CSS
import Text.HTML

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| Where the accumulated count is printed to
export
out : Ref Div
out = Id "reset_out"

||| ID of the increasing button
export
btnInc : Ref Tag.Button
btnInc = Id "reset_inc"

||| ID of the decreasing button
export
btnDec : Ref Tag.Button
btnDec = Id "reset_dec"

||| ID of the reset button
export
btnReset : Ref Tag.Button
btnReset = Id "reset_reset"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

export
resetLbl : String
resetLbl = "reset_resetlbl"

export
incLbl : String
incLbl = "reset_inclbl"

export
decLbl : String
decLbl = "reset_declbl"

export
countLbl : String
countLbl = "reset_countlbl"

export
resetContent : String
resetContent = "reset_content"

export
resetBtn : String
resetBtn = "reset_incbtn"

data Tag = LRes | BRes | LInc | BInc | LDec | BDec | LCnt | OCnt

AreaTag Tag where
  showTag LRes = "LRes"
  showTag BRes = "BRes"
  showTag LInc = "LInc"
  showTag BInc = "BInc"
  showTag LDec = "LDec"
  showTag BDec = "BDec"
  showTag LCnt = "LCnt"
  showTag OCnt = "OCnt"

export
css : List (Rule 1)
css =
  [ class resetContent
      [ Display             $ Area
          (replicate 4 MinContent)
          [MaxContent, MaxContent]
          [ [LRes, BRes]
          , [LInc, BInc]
          , [LDec, BDec]
          , [LCnt, OCnt]
          ]

      , columnGap           $ px 10
      , rowGap              $ px 10
      , padding             $ VH (px 20) (px 10)
      ]

  , class resetLbl  [ gridArea LRes ]

  , ref btnReset  [ gridArea BRes ]

  , class incLbl    [ gridArea LInc ]

  , ref btnInc    [ gridArea BInc ]

  , class decLbl    [ gridArea LDec ]

  , ref btnDec    [ gridArea BDec ]

  , class countLbl  [ gridArea LCnt ]

  , ref out
      [ fontSize        Large
      , gridArea        OCnt
      , textAlign       End
      ]
  ]
