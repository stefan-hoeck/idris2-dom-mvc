module Examples.CSS.Balls

import Data.Vect
import Examples.CSS.Colors
import Text.CSS
import Text.HTML
import public Examples.CSS.Core

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
out : Ref Canvas
out = Id "balls_out"

export
btnRun : Ref Tag.Button
btnRun = Id "balls_run"

export
txtCount : Ref Tag.Input
txtCount = Id "balls_numballs"

export
log : Ref Div
log = Id "balls_log"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

export
ballsContent : String
ballsContent = "balls_content"

export
lblCount : String
lblCount = "balls_lblcount"

data Tag = LNum | INum | BRun | LFPS | Anim | Dot

AreaTag Tag where
  showTag LNum = "LNum"
  showTag INum = "INum"
  showTag BRun = "BRun"
  showTag LFPS = "LFPS"
  showTag Anim = "Anim"
  showTag Dot  = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class ballsContent
          [ display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent]
              [ [LNum, INum]
              , [Dot,  BRun]
              , [LFPS, LFPS]
              , [Anim, Anim]
              ]

          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class ballsContent
          [ display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LNum, INum, Anim]
              , [Dot,  BRun, Anim]
              , [LFPS, LFPS, Anim]
              , [Dot,  Dot,  Anim]
              ]

          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , class lblCount [ gridArea LNum ]

  , ref txtCount
      [ gridArea        INum
      , textAlign       End
      ]

  , ref btnRun [ gridArea BRun ]

  , ref log [ gridArea LFPS ]

  , ref out
      [ justifySelf     Center
      , gridArea        Anim
      , maxWidth        $ px 500
      , width           $ px 500
      ]
  ]
