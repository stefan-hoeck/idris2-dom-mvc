||| CSS Rules for the Math Game Example
module Examples.CSS.MathGame

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Text.CSS
import Web.MVC.ElemRef

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| Where the message about the correct result
||| is printed to.
public export
out : ElemRef
out = Id Div "mathgame_out"

||| Select box where users choose the language.
public export
langIn : ElemRef
langIn = Id Select "mathgame_language"

||| Text field where users enter their result.
public export
resultIn : ElemRef
resultIn = Id Input "mathgame_input"

||| Button to check the entered result
public export
checkBtn : ElemRef
checkBtn = Id Button "mathgame_check_btn"

||| Button to start a new game
public export
newBtn : ElemRef
newBtn = Id Button "mathgame_newbtn"

||| ID of the picture canvas
public export
pic : ElemRef
pic = Id Canvas "mathgame_pic"

||| ID of the calculation label
public export
calc : ElemRef
calc = Id Div "mathgame_calc"

--------------------------------------------------------------------------------
--          CSS
--------------------------------------------------------------------------------

||| Message field class if answer is correct
export
correct : String
correct = "correct"

||| Message field class if answer is wrong
export
wrong : String
wrong = "wrong"

export
mathContent : String
mathContent = "mathgame_content"

export
lblLang : String
lblLang = "mathgame_lbllang"

data Tag = LLan | ILan | OClc | IRes | BChk | ORep | BNew | OPic | Dot

AreaTag Tag where
  showTag LLan = "LLan"
  showTag ILan = "ILan"
  showTag OClc = "OClc"
  showTag IRes = "IRes"
  showTag BChk = "BChk"
  showTag ORep = "ORep"
  showTag BNew = "BNew"
  showTag OPic = "OPic"
  showTag Dot  = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class mathContent
          [ Display           $ Area
              (replicate 6 MinContent)
              [MaxContent, MaxContent]
              [ [LLan, ILan]
              , [OClc, IRes]
              , [Dot,  BChk]
              , [Dot,  BNew]
              , [ORep, ORep]
              , [OPic, OPic]
              ]

          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class mathContent
          [ Display           $ Area
              (replicate 6 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LLan, ILan, OPic]
              , [OClc, IRes, OPic]
              , [Dot,  BChk, OPic]
              , [Dot,  BNew, OPic]
              , [ORep, ORep, OPic]
              , [Dot,  Dot,  OPic]
              ]

          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , class lblLang [ gridArea LLan ]

  , idRef langIn
      [ gridArea        ILan
      , fontSize        Large
      , textAlign       End
      ]

  , idRef calc
      [ gridArea        OClc
      , fontSize        Large
      , textAlign       Start
      ]

  , idRef resultIn
      [ gridArea        IRes
      , fontSize        Large
      , textAlign       End
      ]

  , idRef checkBtn  [ gridArea BChk ]

  , idRef newBtn  [ gridArea BNew ]

  , idRef out
      [ gridArea        ORep
      , fontSize        Large
      , textAlign       Start
      ]

  , idRef pic
      [ backgroundSize  $ perc 100
      , justifySelf     Center
      , gridArea        OPic
      , maxWidth        $ px 500
      , width           $ px 500
      ]

  , class correct [ color green ]

  , class wrong [ color red ]
  ]
