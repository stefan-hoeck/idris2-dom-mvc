||| CSS Rules for the Math Game Example
module Examples.CSS.MathGame

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Text.CSS
import Text.HTML

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| Where the message about the correct result
||| is printed to.
export
out : Ref Div
out = Id "mathgame_out"

||| Select box where users choose the language.
export
langIn : Ref Select
langIn = Id "mathgame_language"

||| Text field where users enter their result.
export
resultIn : Ref Tag.Input
resultIn = Id "mathgame_input"

||| Button to check the entered result
export
checkBtn : Ref Tag.Button
checkBtn = Id "mathgame_check_btn"

||| Button to start a new game
export
newBtn : Ref Tag.Button
newBtn = Id "mathgame_newbtn"

||| ID of the picture canvas
export
pic : Ref Canvas
pic = Id "mathgame_pic"

||| ID of the calculation label
export
calc : Ref Div
calc = Id "mathgame_calc"

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

  , ref langIn
      [ gridArea        ILan
      , fontSize        Large
      , textAlign       End
      ]

  , ref calc
      [ gridArea        OClc
      , fontSize        Large
      , textAlign       Start
      ]

  , ref resultIn
      [ gridArea        IRes
      , fontSize        Large
      , textAlign       End
      ]

  , ref checkBtn  [ gridArea BChk ]

  , ref newBtn  [ gridArea BNew ]

  , ref out
      [ gridArea        ORep
      , fontSize        Large
      , textAlign       Start
      ]

  , ref pic
      [ backgroundSize  $ perc 100
      , justifySelf     Center
      , gridArea        OPic
      , maxWidth        $ px 500
      , width           $ px 500
      ]

  , class correct [ color green ]

  , class wrong [ color red ]
  ]
