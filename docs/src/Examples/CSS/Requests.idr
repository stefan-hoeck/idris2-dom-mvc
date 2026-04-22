module Examples.CSS.Requests

import Data.Vect
import Derive.Prelude
import public Examples.CSS.Core
import Text.CSS
import Text.HTML

%default total
%language ElabReflection

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
requestContent : Class
requestContent = "request-content"

export
requestError : Class
requestError = "request-error"

export
quoteBtn : Class
quoteBtn = "request-quote-btn"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

data Tag = Btn | Quot | Info | Dot

%runElab derive "Tag" [Show,Eq]

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
