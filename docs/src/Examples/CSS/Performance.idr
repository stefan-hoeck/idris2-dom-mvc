module Examples.CSS.Performance

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Text.CSS
import Web.MVC.ElemRef

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

-- displays the current sum of clicks
public export
out : ElemRef
out = Id Div "performance_sum"

-- text fields where users can enter the number of buttons
public export
natIn : ElemRef
natIn = Id Input "performance_numbuttons"

public export
btnRun : ElemRef
btnRun = Id Button "performance_run"

-- where the created buttons go
public export
buttons : ElemRef
buttons = Id Div "performance_buttons"

-- displays the time take to create the buttons
public export
time : ElemRef
time = Id Div "performance_time"

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

  , idRef natIn
      [ gridArea        NBtn
      , textAlign       End
      ]

  , idRef btnRun [ gridArea BRun ]

  , class sumLbl [ gridArea LSum ]

  , idRef out
      [ gridArea        OSum
      , fontSize        Large
      ]

  , idRef time  [ gridArea OTme ]

  , idRef buttons
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
