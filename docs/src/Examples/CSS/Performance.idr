module Examples.CSS.Performance

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Text.CSS
import Text.HTML

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

-- displays the current sum of clicks
export
out : Ref Div
out = Id "performance_sum"

-- text field where users can enter the number of buttons
export
natIn : Ref Tag.Input
natIn = Id "performance_numbuttons"

export
btnRun : Ref Tag.Button
btnRun = Id "performance_run"

-- where the created buttons go
export
buttons : Ref Div
buttons = Id "performance_buttons"

-- displays the time take to create the buttons
export
time : Ref Div
time = Id "performance_time"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

export
inc : String
inc = "performance_inc"

export
numButtonsLbl : String
numButtonsLbl = "performance_numbuttonslbl"

export
sumLbl : String
sumLbl = "performance_sumlbl"

export
grid : String
grid = "performance_grid"

export
performanceContent : String
performanceContent = "performance_content"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

data Tag = LBtn | NBtn | BRun | LSum | OSum | Btns | OTme | Dot

AreaTag Tag where
  showTag LBtn  = "LBtn"
  showTag NBtn  = "NBtn"
  showTag BRun  = "BRun"
  showTag LSum  = "LSum"
  showTag Btns  = "Btns"
  showTag OSum  = "OSum"
  showTag OTme  = "OTme"
  showTag Dot   = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class performanceContent
          [ Display             $ Area
              (replicate 4 MinContent)
              [MaxContent, fr 1, MaxContent]
              [ [LBtn, NBtn, BRun]
              , [LSum, OSum, OSum]
              , [OTme, OTme, OTme]
              , [Btns, Btns, Btns]
              ]
          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class performanceContent
          [ Display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, MaxContent, fr 1]
              [ [LBtn, NBtn, BRun, Btns]
              , [LSum, OSum, OSum, Btns]
              , [OTme, OTme, OTme, Btns]
              , [Dot,  Dot,  Dot,  Btns]
              ]
          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , class numButtonsLbl [ gridArea LBtn ]

  , ref natIn
      [ gridArea        NBtn
      , textAlign       End
      ]

  , ref btnRun [ gridArea BRun ]

  , class sumLbl [ gridArea LSum ]

  , ref out
      [ gridArea        OSum
      , fontSize        Large
      ]

  , ref time  [ gridArea OTme ]

  , ref buttons
      [ gridArea        Btns
      , borderStyle     $ Left Solid
      , borderWidth     $ Left (px 2)
      , borderColor     $ Left base80
      , padding         $ Left (px 10)
      ]

  , class grid
      [ display         Flex
      , flexWrap        "wrap"
      ]

  , class inc
      [ flexBasis       $ perc 5
      , fontSize        XXSmall
      ]
  ]
