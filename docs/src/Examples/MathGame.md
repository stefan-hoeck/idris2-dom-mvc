# For our Children: A simple Math Game

This is a simple math game I wrote for our children.
A picture is hidden behind a grid of panels and they
have to solve randomly generated equations. Every time
an equation is solved correctly, a randomly chosen
tile is removed and part of the picture revealed.

The interactive part of this application is very simple,
but the example demonstrates some other topics that
keep coming up like random value generation and
localization (I'm Swiss, so my children don't yet
speak English).

```idris
module Examples.MathGame

import Data.List
import Data.Vect
import Derive.Prelude
import Examples.CSS.MathGame
import Examples.Util
import JS
import System.Random
import Text.CSS
import Web.MVC
import Web.MVC.Canvas

%language ElabReflection
%default total
```

## Model

We first define the events our application handles. Users
can enter a string and check it against the current
calculation, they can abort and start a new game,
and they can change the language of the user interface:

```idris
data Language = EN | DE

%runElab derive "Language" [Eq]
```

We also need data types for representing the calculations
our users should perform. To keep things simple, we
only support very basic arithmetics:

```idris
data Op = Plus | Minus | Mult

record Calc where
  constructor MkCalc
  x  : Integer
  y  : Integer
  op : Op

record Tile where
  constructor MkTile
  posX    : Bits8
  posY    : Bits8
  calc    : Calc

result : Calc -> Integer
result (MkCalc x y Plus)  = x + y
result (MkCalc x y Minus) = x - y
result (MkCalc x y Mult)  = x * y

dispCalc : Calc -> String
dispCalc (MkCalc x y op) = "\{show x} \{dispOp op} \{show y} = "
  where dispOp : Op -> String
        dispOp Plus  = "+"
        dispOp Minus = "-"
        dispOp Mult  = "*"

public export
data MathEv : Type where
  Lang     : Language -> MathEv
  Check    : MathEv
  MathInit : MathEv
  NewGame  : List Tile -> String -> MathEv
  Inp      : String -> MathEv

lang : String -> MathEv
lang "de" = Lang DE
lang _    = Lang EN
```

Next, we need to keep track of the current game state:
The current calculation to solve, the tiles on the
picture that have already been removed, the stuck tiles
from wrong answers, and the picture (represented as
the image's URL) hidden behind the tiles. In order not
to interleave controller code with
code for generating new random calculations, we generate the
whole game in advance, pairing calculations with the
corresponding tiles covering the picture.

```idris
data Result : Type where
  Ended   : Result
  Correct : Result
  Wrong   : Calc -> Integer -> Result

public export
record MathST where
  constructor MS
  lang   : Language
  answer : String
  result : Maybe Result
  rows   : Bits8
  wrong  : List Tile
  calcs  : List Tile
  pic    : String

export
init : MathST
init = MS EN "" Nothing 4 [] [] ""

currentCalc : MathST -> Maybe Calc
currentCalc gs = case gs.calcs of
  t :: _ => Just t.calc
  Nil    => Nothing
```

Finally, we need a list of pictures from which we
randomly choose one. These were all taken from
[pexels](https://www.pexels.com/search/open%20source/)
and cropped and scaled down to 500 x 500 pixels:

```idris
pictures : List String
pictures = map (\n => "pics/pic\{show n}.jpg") [S Z .. 11]
```

## View

As usual, the application's CSS rules have been moved to
a [separate module](CSS/MathGame.idr). We start with defining
the localized strings we are going to need:

```idris
style : Result -> Maybe String
style Ended       = Nothing
style Correct     = Just "color : \{green}"
style (Wrong _ _) = Just "color : \{red}"

language : Language -> String
language DE = "Sprache"
language EN = "Language"

german : Language -> String
german DE = "Deutsch"
german EN = "German"

english : Language -> String
english DE = "Englisch"
english EN = "English"

resultStr : Language -> String
resultStr DE = "Resultat"
resultStr EN = "result"

checkAnswerStr : Language -> String
checkAnswerStr DE = "Antwort prüfen"
checkAnswerStr EN = "Check answer"

newGameStr : Language -> String
newGameStr DE = "Neues Spiel"
newGameStr EN = "New game"

reply : Language -> Result -> String
reply EN Ended   = "The game has ended."
reply EN Correct = "Correct!"
reply DE Ended   = "Das Spiel ist vorbei."
reply DE Correct = "Richtig!"
reply EN (Wrong c n) =
     "That's not correct. Your answer was \{show n}. "
  ++ "The correct answer is: \{dispCalc c} \{show $ result c}."
reply DE (Wrong c n) =
     "Leider falsch. Deine Antwort war \{show n}. "
  ++ "Die richtige Antwort ist: \{dispCalc c} \{show $ result c}."
```

We can now define the HTML elements of the application:

```idris
wcanvas : Bits32
wcanvas = 500

content : Language -> Node MathEv
content l =
  div [ class mathContent ]
    [ lbl "\{language l}:" lblLang
    , select
        [ Id langIn, classes [widget, selectIn], onChange lang]
        [ option [ value "de", selected (l == DE)] [Text $ german l]
        , option [ value "en", selected (l == EN)] [Text $ english l]
        ]

    , div [ Id calc ] []

    , input [ Id resultIn
            , onEnterDown Check
            , onInput Inp
            , class widget
            , placeholder (resultStr l)
            ] []

    , button [ Id checkBtn
             , onClick Check
             , classes [widget,btn]
             ] [Text $ checkAnswerStr l]

    , button [ Id newBtn
             , onClick MathInit
             , classes [widget,btn]
             ] [Text $ newGameStr l]

    , div [ Id out ] []

    , canvas [ Id pic, width wcanvas, height wcanvas ] []
    ]
```

We also need to display the tiles still hiding the
picture, distinguishing between *stuck* tiles from wrong
answers and initial tiles from calculations not yet
solved:

```idris
tile : Tile -> Scene
tile t = S1 [] Id $ Rect (cast t.posX) (cast t.posY) 1 1 Fill

stuckColor : Color
stuckColor = HSLA 0 0 50 80

dispState : MathST -> Scene
dispState gs =
  let sf = cast {to = Double} wcanvas / cast gs.rows
   in SM [] (scale sf sf)
        [ SM [ Fill black ] Id $ map tile gs.calcs
        , SM [ Fill stuckColor ] Id $ map tile gs.wrong
        ]
```

## Controller

For controlling the game, we first need a way to
randomly generate equations and shuffled lists of
tiles. I define an arbitrary upper bound of 100 for
the results and values used in the equations.

In order to shuffle the list of tiles, we pair them
with randomly generated numbers and use those for
sorting the list:

```idris
upperBound : Int32
upperBound = 100

randomCalc : HasIO io => io Calc
randomCalc = do
  op   <- rndSelect' [Plus,Minus,Mult]
  case op of
    Plus => do
      x <- randomRIO (0, upperBound)
      y <- randomRIO (0, upperBound - x)
      pure $ MkCalc (cast x) (cast y) op

    Minus => do
      x <- randomRIO (0, upperBound)
      y <- randomRIO (0, x)
      pure $ MkCalc (cast x) (cast y) op

    Mult => do
      x <- randomRIO (1, 12)
      y <- randomRIO (0, upperBound `div` x)
      pure $ MkCalc (cast x) (cast y) op

randomTile : HasIO io => (Bits8,Bits8) -> io (Int32, Tile)
randomTile (px,py) = do
  c       <- randomCalc
  sortVal <- randomRIO (0, 1000)
  pure (sortVal, MkTile px py c)

randomGame : HasIO io => io MathEv
randomGame = do
  pic   <- rndSelect pictures
  pairs <- traverse randomTile [| MkPair [the Bits8 0..3] [the Bits8 0..3] |]
  let ts = snd <$> sortBy (comparing fst) pairs
  pure $ NewGame ts pic
```

The heart of the application logic is function `checkAnswer`:

```idris
checkAnswer : MathST -> MathST
checkAnswer (MS l input _ nr wrong (h :: t) pic) =
  let ans := cast {to = Integer} input
   in if result h.calc == ans
         then MS l "" (Just Correct) nr wrong t pic
         else MS l "" (Just $ Wrong h.calc ans) nr (h::wrong) t pic
checkAnswer gs = {result := Just Ended} gs
```

With the above, updating the application state is very easy:

```idris
export
update : MathEv -> MathST -> MathST
update (Lang x)         s = {lang := x} s
update Check            s = checkAnswer s
update MathInit         s = {lang := s.lang} init
update (Inp a)          s = {answer := a} s
update (NewGame ts pic) s =
  { answer := ""
  , result := Nothing
  , wrong  := []
  , calcs  := ts
  , pic    := pic
  } s
```

Next, we define the functionality used to display the game state.
We make sure to properly set the canvas background image,
disable components if the game has ended, render the tiles on
top of the picture, display the next calculation, and clear
the input text field:

```idris
displayST : MathST -> Cmd MathEv
displayST s =
  batch
    [ disabledM checkBtn $ currentCalc s
    , disabledM resultIn $ currentCalc s
    , text calc $ maybe "" dispCalc (currentCalc s)
    , text out  $ maybe "" (reply s.lang) s.result
    , attr pic  $ style "background-image : url('\{s.pic}');"
    , attr out  $ style (fromMaybe "" $ s.result >>= style)
    , render pic (dispState s)
    ]

displayEv : MathEv -> Cmd MathEv
displayEv (Lang x)      = child exampleDiv (content x)
displayEv Check         = value resultIn ""
displayEv MathInit      = child exampleDiv (content EN) <+> cmd randomGame
displayEv (Inp _)       = noAction
displayEv (NewGame _ _) = child exampleDiv (content init.lang)

export covering
display : MathEv -> MathST -> Cmd MathEv
display e s = displayEv e <+> displayST s
```

One important aspect I'd like to point out is that coming up with a new
game is an effectful computation, because it involves random value
generation. For this, we synchronously run `randomGame` and fire the resulting
event when we encounter a `MathInit` event (`cmd randomGame`).

<!-- vi: filetype=idris2:syntax=markdown
-->
