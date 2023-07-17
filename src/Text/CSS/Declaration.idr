module Text.CSS.Declaration

import Data.List
import Text.CSS.Color
import Text.CSS.Dir
import Text.CSS.Flexbox
import Text.CSS.Gradient
import Text.CSS.Grid
import Text.CSS.Length
import Text.CSS.ListStyleType
import Text.CSS.Percentage
import Text.CSS.Property

%default total

public export
data Declaration : Type where
  Decl : (property, value : String) -> Declaration
  Display : Display -> Declaration

export
Interpolation Declaration where
  interpolate (Decl p v) = "\{p}: \{v};"
  interpolate (Display Flex) = "display: flex;"
  interpolate (Display Grid) = "display: grid;"
  interpolate (Display $ Area rs cs a) = "\{renderArea rs cs a};"

export %inline
decl : Interpolation a => String -> a -> Declaration
decl s = Decl s . interpolate

-- prefix
prfx : Dir a -> String
prfx (Left _)   = "-left"
prfx (Right _)  = "-right"
prfx (Top _)    = "-top"
prfx (Bottom _) = "-bottom"
prfx _          = ""

export
dirDecl : (prop : String) -> (a -> String) -> Dir a -> Declaration
dirDecl prop f d =
  let vs := concat . intersperse " " . map f $ vals d
   in Decl "\{prop}\{prfx d}" vs

export
dirDecl2 : (prop,suffix : String) -> (a -> String) -> Dir a -> Declaration
dirDecl2 prop suffix f d =
  let vs := concat . intersperse " " . map f $ vals d
   in Decl "\{prop}\{prfx d}-\{suffix}" vs

--------------------------------------------------------------------------------
--          Predefined Properties
--------------------------------------------------------------------------------

export %inline
alignItems : FlexAlign -> Declaration
alignItems = decl "align-items"

export %inline
alignSelf : FlexAlign -> Declaration
alignSelf = decl "align-self"

export %inline
backgroundColor : Color -> Declaration
backgroundColor = decl "background-color"

export %inline
backgroundImage : (url : String) -> Declaration
backgroundImage = Decl "background-image"

export %inline
backgroundImageGradient : Gradient -> Declaration
backgroundImageGradient = decl "background-image"

export %inline
backgroundSize : Width -> Declaration
backgroundSize = decl "background-size"

export %inline
border : String -> Declaration
border = Decl "border"

export %inline
borderColor : Dir Color -> Declaration
borderColor = dirDecl2 "border" "color" interpolate

export %inline
borderRadius : BorderRadius -> Declaration
borderRadius = decl "border-radius"

export %inline
borderStyle : Dir BorderStyle -> Declaration
borderStyle = dirDecl2 "border" "style" interpolate

export %inline
borderWidth : Dir BorderWidth -> Declaration
borderWidth = dirDecl2 "border" "width" interpolate

export %inline
boxSizing : BoxSizing -> Declaration
boxSizing = decl "box-sizing"

export %inline
color : Color -> Declaration
color = decl "color"

export %inline
columnGap : Length -> Declaration
columnGap = decl "column-gap"

export %inline
direction : Direction -> Declaration
direction = decl "direction"

export %inline
display : Display -> Declaration
display = Display

export %inline
flex : String -> Declaration
flex = Decl "flex"

export %inline
flexBasis : FlexBasis -> Declaration
flexBasis = decl "flex-basis"

export %inline
flexDirection : FlexDirection -> Declaration
flexDirection = decl "flex-direction"

export %inline
flexWrap : String -> Declaration
flexWrap = Decl "flex-wrap"

export %inline
flexGrow : Nat -> Declaration
flexGrow = Decl "flex-grow" . show

export %inline
flexFlow : List FlexFlow -> Declaration
flexFlow = decl "flex-flow"

export %inline
fontFamily : String -> Declaration
fontFamily = Decl "font-family"

export %inline
fontSize : FontSize -> Declaration
fontSize = decl "font-size"

export %inline
fontWeight : FontWeight -> Declaration
fontWeight = decl "font-weight"

export %inline
gridArea : AreaTag a => a -> Declaration
gridArea = Decl "grid-area" . showTag

export %inline
gridColumn : GridPosition -> Declaration
gridColumn = decl "grid-column"

export %inline
gridRow : GridPosition -> Declaration
gridRow = decl "grid-row"

export %inline
gridTemplateColumns : List GridValue -> Declaration
gridTemplateColumns = decl "grid-template-columns"

export %inline
gridTemplateRows : List GridValue -> Declaration
gridTemplateRows = decl "grid-template-rows"

export %inline
height : Width -> Declaration
height = decl "height"

export %inline
justifyContent : FlexJustify -> Declaration
justifyContent = decl "justify-content"

export %inline
justifySelf : FlexJustify -> Declaration
justifySelf = decl "justify-self"

export %inline
listStyleType : ListStyleType -> Declaration
listStyleType = decl "list-style-type"

export %inline
margin : Dir Length -> Declaration
margin = dirDecl "margin" interpolate

export %inline
maxHeight : Width -> Declaration
maxHeight = decl "max-height"

export %inline
maxWidth : Width -> Declaration
maxWidth = decl "max-width"

export %inline
minHeight : Width -> Declaration
minHeight = decl "min-height"

export %inline
minWidth : Width -> Declaration
minWidth = decl "min-width"

export %inline
overflowX : Overflow -> Declaration
overflowX = decl "overflow-x"

export %inline
overflowY : Overflow -> Declaration
overflowY = decl "overflow-y"

export %inline
padding : Dir Length -> Declaration
padding = dirDecl "padding" interpolate

export %inline
rowGap : Length -> Declaration
rowGap = decl "row-gap"

export %inline
textAlign : TextAlign -> Declaration
textAlign = decl "text-align"

export %inline
textDecoration : String -> Declaration
textDecoration = Decl "text-decoration"

export %inline
textDecorationColor : Color -> Declaration
textDecorationColor = decl "text-decoration-color"

export %inline
textDecorationLine : TextDecorationLine -> Declaration
textDecorationLine = decl "text-decoration-line"

export %inline
textDecorationStyle : TextDecorationStyle -> Declaration
textDecorationStyle = decl "text-decoration-style"

export %inline
textOverflow : TextOverflow -> Declaration
textOverflow = decl "text-overflow"

export %inline
textOverflow2 : TextOverflow -> TextOverflow -> Declaration
textOverflow2 x y = Decl "text-overflow" "\{x} \{y}"

export %inline
width : Width -> Declaration
width = decl "width"

export %inline
whitespace : WhiteSpace -> Declaration
whitespace = decl "white-space"
