module Examples.CSS.Fractals

import Data.Vect
import Examples.CSS.Colors
import public Examples.CSS.Core
import Text.CSS
import Web.MVC.ElemRef

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

public export
out : ElemRef
out = Id Div "fractals_out"

public export
btnRun : ElemRef
btnRun = Id Button "fractals_run"

public export
txtIter : ElemRef
txtIter = Id Input "fractals_iterations"

public export
txtRedraw : ElemRef
txtRedraw = Id Input "fractals_redrawdelay"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

export
fractalContent : String
fractalContent = "fractals_content"

export
lblIter : String
lblIter = "fractals_lbliter"

export
lblDelay : String
lblDelay = "fractals_lbldelay"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

data Tag = LIter | IIter | LDel | IDel | BRun | Fract | Dot

AreaTag Tag where
  showTag LIter = "LIter"
  showTag IIter = "IIter"
  showTag LDel  = "LDel"
  showTag IDel  = "IDel"
  showTag BRun  = "BRun"
  showTag Fract = "Fract"
  showTag Dot   = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class fractalContent
          [ Display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent]
              [ [LIter, IIter]
              , [LDel,  IDel ]
              , [Dot,   BRun ]
              , [Fract, Fract]
              ]

          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , Media "min-width: 800px"
      [ class fractalContent
          [ Display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LIter, IIter, Fract]
              , [LDel,  IDel,  Fract]
              , [Dot,   BRun,  Fract]
              , [Dot,   Dot,   Fract]
              ]

          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]
  , class lblIter [ gridArea LIter ]

  , idRef txtIter
      [ gridArea        IIter
      , textAlign       End
      ]

  , class lblDelay [ gridArea LDel ]

  , idRef txtRedraw
      [ gridArea        IDel
      , textAlign       End
      ]

  , idRef btnRun [ gridArea BRun ]

  , idRef out
      [ justifySelf     Center
      , gridArea        Fract
      , borderStyle     $ Left Solid
      , borderWidth     $ Left (px 2)
      , borderColor     $ Left base80
      , maxWidth        $ px 500
      , width           $ px 500
      ]
  ]
