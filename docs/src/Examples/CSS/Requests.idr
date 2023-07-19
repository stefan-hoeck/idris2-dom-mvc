module Examples.CSS.Requests

import Data.Vect
import public Examples.CSS.Core
import Text.CSS
import Text.HTML

%default total

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
quote : Ref Blockquote
quote = Id "request-quote"

export
quoteInfo : Ref Tag.P
quoteInfo = Id "request-quote-info"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

export
requestContent : String
requestContent = "request-content"

export
requestError : String
requestError = "request-error"

export
quoteBtn : String
quoteBtn = "request-quote-btn"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

data Tag = Btn | Quot | Info | Dot

AreaTag Tag where
  showTag Btn  = "Btn"
  showTag Quot = "Quot"
  showTag Info = "Info"
  showTag Dot  = "."

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class requestContent
          [ Display             $ Area
              [MinContent, MaxContent, MaxContent]
              [px 200, px 400]
              [ [Btn, Dot]
              , [Quot, Quot]
              , [Info, Info]
              ]
          , columnGap           $ px 10
          , rowGap              $ px 10
          , padding             $ VH (px 20) (px 10)
          ]
      ]

  , ref quote     [gridArea Quot]

  , ref quoteInfo [gridArea Info, textAlign End]

  , class quoteBtn [gridArea Btn]
  ]
