||| CSS Rules for the Inc. Buttons Example
module Examples.CSS.Reset

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Text.CSS
import Web.MVC.ElemRef

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| Where the accumulated count is printed to
public export
out : ElemRef
out = Id Div "reset_out"

||| ID of the increasing button
public export
btnInc : ElemRef
btnInc = Id Button "reset_inc"

||| ID of the decreasing button
public export
btnDec : ElemRef
btnDec = Id Button "reset_dec"

||| ID of the reset button
public export
btnReset : ElemRef
btnReset = Id Button "reset_reset"

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

  , idRef btnReset  [ gridArea BRes ]

  , class incLbl    [ gridArea LInc ]

  , idRef btnInc    [ gridArea BInc ]

  , class decLbl    [ gridArea LDec ]

  , idRef btnDec    [ gridArea BDec ]

  , class countLbl  [ gridArea LCnt ]

  , idRef out
      [ fontSize        Large
      , gridArea        OCnt
      , textAlign       End
      ]
  ]
