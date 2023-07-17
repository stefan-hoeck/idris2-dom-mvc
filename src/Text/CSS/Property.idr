module Text.CSS.Property

import Data.List
import Data.String
import Data.Vect
import Text.CSS.Color
import Text.CSS.Dir
import Text.CSS.Flexbox
import Text.CSS.Grid
import Text.CSS.Length
import Text.CSS.ListStyleType
import Text.CSS.Percentage

%default total

namespace BoxSizing
  public export
  data BoxSizing = BorderBox | ContentBox

  export
  Interpolation BoxSizing where
    interpolate BorderBox  = "border-box"
    interpolate ContentBox = "content-box"

namespace Direction
  public export
  data Direction = LTR | RTL

  export
  Interpolation Direction where
    interpolate LTR = "ltr"
    interpolate RTL = "rtl"

namespace Display
  public export
  interface AreaTag a where
    showTag : a -> String

  public export
  data Display : Type where
    Flex  : Display
    Grid  : Display
    Area  :  {0 n,m : Nat}
          -> {0 a : Type}
          -> AreaTag a
          => (rows    : Vect (S m) GridValue)
          -> (columns : Vect (S n) GridValue)
          -> (area    : Vect (S m) (Vect (S n) a))
          -> Display

  export
  renderArea :  AreaTag a
             => Vect (S m) GridValue
             -> Vect (S n) GridValue
             -> Vect (S m) (Vect (S n) a)
             -> String
  renderArea rs cs as =
    let rsStr = "grid-template-rows: \{toList rs}"
        csStr = "grid-template-columns: \{toList cs}"
        aStr  = fastConcat . intersperse " " . map col $ toList as
     in "display: grid; \{rsStr}; \{csStr}; grid-template-areas: \{aStr}"
    where col : Vect (S n) a -> String
          col vs =
            let str = concat . intersperse " " . map showTag $ toList vs
             in #""\#{str}""#

namespace FlexBasis
  public export
  data FlexBasis : Type where
    FL       : Length -> FlexBasis
    FP       : Percentage -> FlexBasis

  export
  Interpolation FlexBasis where
    interpolate (FL x)   = interpolate x
    interpolate (FP x)   = interpolate x

  export %inline
  Cast Length FlexBasis where
    cast = FL

  export %inline
  Cast Percentage FlexBasis where
    cast = FP

namespace FontSize
  public export
  data FontSize : Type where
    FL       : Length -> FontSize
    FP       : Percentage -> FontSize
    XXSmall  : FontSize
    XSmall   : FontSize
    Small    : FontSize
    Medium   : FontSize
    Large    : FontSize
    XLarge   : FontSize
    XXLarge  : FontSize
    XXXLarge : FontSize

  export
  Interpolation FontSize where
    interpolate (FL x)   = interpolate x
    interpolate (FP x)   = interpolate x
    interpolate XXSmall  = "xx-small"
    interpolate XSmall   = "x-small"
    interpolate Small    = "small"
    interpolate Medium   = "medium"
    interpolate Large    = "large"
    interpolate XLarge   = "x-large"
    interpolate XXLarge  = "xx-large"
    interpolate XXXLarge = "xxx-large"

  export %inline
  Cast Length FontSize where
    cast = FL

  export %inline
  Cast Percentage FontSize where
    cast = FP

namespace FontWeight
  public export
  data FontWeight : Type where
    Normal  : FontWeight
    Bold    : FontWeight
    Lighter : FontWeight
    Bolder  : FontWeight
    FW100   : FontWeight
    FW200   : FontWeight
    FW300   : FontWeight
    FW400   : FontWeight
    FW500   : FontWeight
    FW600   : FontWeight
    FW700   : FontWeight
    FW800   : FontWeight
    FW900   : FontWeight

  export
  Interpolation FontWeight where
    interpolate Normal  = "normal"
    interpolate Bold    = "bold"
    interpolate Lighter = "lighter"
    interpolate Bolder  = "bolder"
    interpolate FW100   = "100"
    interpolate FW200   = "200"
    interpolate FW300   = "300"
    interpolate FW400   = "400"
    interpolate FW500   = "500"
    interpolate FW600   = "600"
    interpolate FW700   = "700"
    interpolate FW800   = "800"
    interpolate FW900   = "900"

namespace BorderRadius
  public export
  data BorderRadius : Type where
    BL : Length -> BorderRadius
    BP : Percentage -> BorderRadius
    BS : String -> BorderRadius

  export
  Interpolation BorderRadius where
    interpolate (BL x) = interpolate x
    interpolate (BP x) = interpolate x
    interpolate (BS x) = x

  export %inline
  Cast Length BorderRadius where
    cast = BL

  export %inline
  Cast Percentage BorderRadius where
    cast = BP

  export %inline
  FromString BorderRadius where
    fromString = BS

namespace BorderStyle
  public export
  data BorderStyle : Type where
    None   : BorderStyle
    Hidden : BorderStyle
    Dotted : BorderStyle
    Dashed : BorderStyle
    Solid  : BorderStyle
    Dbl    : BorderStyle
    Groove : BorderStyle
    Ridge  : BorderStyle
    Inset  : BorderStyle
    Outset : BorderStyle


  export
  Interpolation BorderStyle where
    interpolate None   = "none"
    interpolate Hidden = "hidden"
    interpolate Dotted = "dotted"
    interpolate Dashed = "dashed"
    interpolate Solid  = "solid"
    interpolate Dbl    = "double"
    interpolate Groove = "groove"
    interpolate Ridge  = "ridge"
    interpolate Inset  = "inset"
    interpolate Outset = "outset"

namespace BorderWidth
  public export
  data BorderWidth : Type where
    BL     : Length -> BorderWidth
    Thin   : BorderWidth
    Medium : BorderWidth
    Thick  : BorderWidth

  export
  Interpolation BorderWidth where
    interpolate (BL x) = interpolate x
    interpolate Thin   = "thin"
    interpolate Medium = "medium"
    interpolate Thick  = "thick"

  export %inline
  Cast Length BorderWidth where
    cast = BL

namespace Overflow

  ||| The `overflow-x` and `overflow-y` CSS properties set
  ||| what shows when content overflows a block-level element's
  ||| edges. This may be nothing, a scroll bar,
  ||| or the overflow content.
  public export
  data Overflow : Type where

    ||| Content is not clipped and may be rendered outside the padding box's edges.
    Visible : Overflow

    ||| Content is clipped if necessary to fit horizontally
    ||| in the padding box. No scrollbars are provided.
    ||| Programmatic scrolling is still possible.
    Hidden  : Overflow

    ||| Like `Hidden` but forbidding also programmatic scrolling.
    Clip    : Overflow

    ||| Content is clipped if necessary to fit in the padding box.
    ||| Browsers display scrollbars whether or not any content is actually clipped.
    ||| (This prevents scrollbars from appearing or disappearing when the
    ||| content changes.)
    Scroll  : Overflow

    ||| Depends on the user agent. If content fits inside the
    ||| padding box, it looks the same as visible, but still
    ||| establishes a new block-formatting context. Desktop browsers
    ||| provide scrollbars if content overflows.
    Auto    : Overflow

  export
  Interpolation Overflow where
    interpolate Visible = "visible"
    interpolate Hidden  = "hidden"
    interpolate Clip    = "clip"
    interpolate Scroll  = "scroll"
    interpolate Auto    = "auto"

namespace TextAlign
  public export
  data TextAlign : Type where
    ||| The same as left if direction is left-to-right and right if direction is right-to-left.
    Start   : TextAlign
    ||| The same as right if direction is left-to-right and left if direction is right-to-left.
    End     : TextAlign
    ||| The inline contents are aligned to the left edge of the line box.
    Left    : TextAlign
    ||| The inline contents are aligned to the right edge of the line box.
    Right   : TextAlign
    ||| The inline contents are centered within the line box.
    Center  : TextAlign
    ||| The inline contents are justified. Text should be spaced to line up its left and right edges to the left and right edges of the line box, except for the last line.
    Justify : TextAlign

  export
  Interpolation TextAlign where
    interpolate Start   = "start"
    interpolate End     = "end"
    interpolate Left    = "left"
    interpolate Right   = "right"
    interpolate Center  = "center"
    interpolate Justify = "justify"

namespace TextDecorationLine

  public export
  data TextDecorationLine : Type where
    None        : TextDecorationLine
    Underline   : TextDecorationLine
    Overline    : TextDecorationLine
    LineThrough : TextDecorationLine

  export
  Interpolation TextDecorationLine where
    interpolate None        = "none"
    interpolate Underline   = "underline"
    interpolate Overline    = "overline"
    interpolate LineThrough = "line-through"

namespace TextDecorationStyle

  public export
  data TextDecorationStyle : Type where
    Solid  : TextDecorationStyle
    Dbl    : TextDecorationStyle
    Dotted : TextDecorationStyle
    Dashed : TextDecorationStyle
    Wavy   : TextDecorationStyle

  export
  Interpolation TextDecorationStyle where
    interpolate Solid  = "solid"
    interpolate Dbl    = "double"
    interpolate Dotted = "dotted"
    interpolate Dashed = "dashed"
    interpolate Wavy   = "wavy"

namespace TextOverflow
  public export
  data TextOverflow = Clip | Ellipsis

  export
  Interpolation TextOverflow where
    interpolate Clip     = "clip"
    interpolate Ellipsis = "ellipsis"

namespace Width
  public export
  data Width : Type where
    WL       : Length -> Width
    WP       : Percentage -> Width

  export
  Interpolation Width where
    interpolate (WL x)   = interpolate x
    interpolate (WP x)   = interpolate x

  export %inline
  Cast Length Width where
    cast = WL

  export %inline
  Cast Percentage Width where
    cast = WP

namespace WiteSpace

  ||| Handles white space inside an element
  public export
  data WhiteSpace : Type where
    ||| White space is collapsed (several white space characters
    ||| are treated as a single space character) and lines are
    ||| broken as necessary.
    Normal      : WhiteSpace

    ||| Like `Normal` but suppresses line breaks (text wrapping)
    Nowrap      : WhiteSpace

    ||| Sequences of white space are preserved. Lines are broken at
    ||| newline characters and at `<br>` elements.
    Pre         : WhiteSpace

    ||| Sequences of white space are preserved. Lines are broken at
    ||| newline characters, at `<br>` elements, and as necessary
    ||| to fill the box.
    PreWrap     : WhiteSpace

    ||| Like `PreWrap` but collapses sequences of white space.
    PreLine     : WhiteSpace

    ||| The behavior is identical to that of `PreWrap`, except that:
    |||
    ||| * Any sequence of preserved white space always takes up space,
    |||   including at the end of the line.
    ||| * A line breaking opportunity exists after every preserved
    |||   white space character, including between white space characters.
    ||| * Such preserved spaces take up space and do not hang,
    |||   and thus affect the box's intrinsic sizes
    |||   (min-content size and max-content size).
    BreakSpaces : WhiteSpace

  export
  Interpolation WhiteSpace where
    interpolate Normal      = "normal"
    interpolate Nowrap      = "nowrap"
    interpolate Pre         = "pre"
    interpolate PreWrap     = "pre-wrap"
    interpolate PreLine     = "pre-line"
    interpolate BreakSpaces = "break-spaces"
