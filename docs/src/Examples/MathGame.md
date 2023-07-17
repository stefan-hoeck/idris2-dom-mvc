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
chan enter a string and check it against the currect
calculation, they can abort and start a new game,
and they can change the UI's language:

```idris
data Language = EN | DE

%runElab derive "Language" [Show,Eq]

public export
data Ev = Lang String | Check | NewGame

%runElab derive "Ev" [Show,Eq]
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
```

Next, we need to keep track of the current game state:
The current calculation to solve, the tiles on the
picture that have already been removed, the stuck tiles
from wrong answers, and the picture (represented as
the image's URL) hidden behind the tiles. In order not
to interleave controller code (stream functions) with
for generating new random calculations, we generate the
whole game in advance, pairing calculations with the
corresponding tiles covering the picture.

```idris
record Tile where
  constructor MkTile
  posX    : Bits8
  posY    : Bits8
  calc    : Calc

record GameState where
  constructor MkGS
  lang   : Language
  rows   : Bits8
  wrong  : List Tile
  calcs  : List Tile
  pic    : String

currentCalc : GameState -> Maybe Calc
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
pictures = map (\n => "pics/pic\{show n}.jpg") [the Bits8 1..11]
```

## View

As usual, the application's CSS rules have been moved to
a [separate module](CSS/MathGame.idr). We start with defining
the localized strings we need:

```idris
data Result = Ended Language
            | Correct Language
            | Wrong Language Calc Integer

style : Result -> Maybe String
style (Ended _)     = Nothing
style (Correct _)   = Just "color : \{green}"
style (Wrong _ _ _) = Just "color : \{red}"

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

reply : Result -> String
reply (Ended EN)   = "The game has ended."
reply (Correct EN) = "Correct!"
reply (Ended DE)   = "Das Spiel ist vorbei."
reply (Correct DE) = "Richtig!"
reply (Wrong EN c n) =
     "That's not correct. Your answer was \{show n}. "
  ++ "The correct answer is: \{dispCalc c} \{show $ result c}."
reply (Wrong DE c n) =
     "Leider falsch. Deine Antwort war \{show n}. "
  ++ "Die richtige Antwort ist: \{dispCalc c} \{show $ result c}."
```

We can now define the HTML elements of the application:

```idris
wcanvas : Bits32
wcanvas = 500

content : Language -> Node Ev
content l =
  div [ class mathContent ]
    [ lbl "\{language l}:" lblLang
    , select
        [ ref langIn, classes [widget, selectIn], onChange Lang]
        [ option [ value "de", selected (l == DE)] [Text $ german l]
        , option [ value "en", selected (l == EN)] [Text $ english l]
        ]

    , div [ ref calc ] []

    , input [ ref resultIn
            , onEnterDown Check
            , class widget
            , placeholder (resultStr l)
            ] []

    , button [ ref checkBtn
             , onClick Check
             , classes [widget,btn]
             ] [Text $ checkAnswerStr l]

    , button [ ref newBtn
             , onClick NewGame
             , classes [widget,btn]
             ] [Text $ newGameStr l]

    , div [ ref out ] []

    , canvas [ ref pic, width wcanvas, height wcanvas ] []
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

dispState : GameState -> Scene
dispState gs =
  let sf = cast {to = Double} wcanvas / cast gs.rows
   in SM [] (scale sf sf)
        [ SM [ Fill black ] Id $ map tile gs.calcs
        , SM [ Fill stuckColor ] Id $ map tile gs.wrong
        ]

renderGame : GameState -> JSIO ()
renderGame gs =
  render $ MkCanvas pic (cast wcanvas) (cast wcanvas) (dispState gs)
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

randomGame : HasIO io => Language -> io GameState
randomGame l = do
  pic   <- rndSelect pictures
  pairs <- traverse randomTile [| MkPair [0..3] [0..3] |]
  let ts = snd <$> sortBy (comparing fst) pairs
  pure $ MkGS l 4 Nil ts pic
```

For implementing the wirings of the application, we first
note that we need to keep track of the current game's state.
We therefore use the `StateT` monad transformer. We will see
below, how we can easily break out of the state monad and
convert corresponding MSFs to one, which take and yield
an additional value.

The heart of the application logic is function `checkAnswer`:
It takes a pair of the user input and the current game state
and returns a `Result` plus the updated state:

```idris
checkAnswer : String -> GameState -> HList [Result,GameState]
checkAnswer s (MkGS l nr wrong (h :: t) pic) =
  let answer = cast s
   in if result h.calc == answer
      then [Correct l, MkGS l nr wrong t pic]
      else [Wrong l h.calc answer, MkGS l nr (h :: wrong) t pic]
checkAnswer s gs = [Ended gs.lang, gs]
```

Next, we define the functionality used to display the game state.
We make sure to properly set the canvas background image,
disable components if the game has ended, render the tiles on
top of the picture, display the next calculation, and clear
the input text field:

```idris
-- setPic : MSF JSIO GameState ()
-- setPic =   (\gs => "background-image : url('\{gs.pic}');")
--        ^>> attributeAt_ "style" pic
--
-- dispGame : MSF JSIO GameState ()
-- dispGame =
--   fan_ [ currentCalc ^>-
--                     [ isNothing ^>- [disabledAt checkBtn, disabledAt resultIn]
--                     , maybe "" dispCalc ^>> text calc
--                     ]
--        , arrM renderGame
--        , const "" >>> Sink.valueOf resultIn
--        , setPic
--        ]
```

For checking answers entered by users, we need a stream function
over a `StateT` transformer. The alternative would be to pair
the input and output type of the MSF with `GameState`, and indeed
the two forms are interconvertible (see `fromState` and `toState`
from `Data.MSF.Trans`).

```idris
-- check : ST () GameState => MSF JSIO i ()
-- check =  [| checkAnswer (valueOf resultIn) (constM $ getAt ()) |]
--       >>- [ snd >>! setAt ()
--           , hd  >>> reply ^>> text out
--           , hd  >>> style ^>> attributeAt "style" out
--           ]
```

When creating a new game, we need to make sure to take over
the current language setting. Likewise, adjusting the language
should overwrite the corresponding field of the game state
and redraw the UI:

```idris
-- newGame : ST () GameState => MSF JSIO i ()
-- newGame =
--       constM (getAt ())
--   >>> lang
--   ^>> randomGame
--   !>- [setPic, arrM $ setAt ()]
--
-- adjLang : Handler JSIO Ev => ST () GameState => MSF JSIO Ev ()
-- adjLang = readLang ^>> ifJust (
--             arrM $ \l => innerHtmlAt exampleDiv (content l)
--                       >> modifyAt () { lang := l }
--           )
--   where readLang : Ev -> Maybe Language
--         readLang (Lang "en") = Just EN
--         readLang (Lang "de") = Just DE
--         readLang _           = Nothing
```

We put everything together in the main controller, which
just broadcasts the current event to the sub-controllers:

```idris
-- msf : Handler JSIO Ev => ST () GameState => MSF JSIO Ev ()
-- msf =  fan_ [ ifIs NewGame newGame
--             , ifIs Check check
--             , adjLang
--             , constM (getAt ()) >>> dispGame
--             ]
--
-- export
-- ui : Handler JSIO Ev => JSIO (MSF JSIO Ev (), JSIO ())
-- ui = do
--   ini <- randomGame DE
--   _ <- mkST ini
--   innerHtmlAt exampleDiv (content DE)
--   pure (msf, pure ())
```

<!-- vi: filetype=idris2:syntax=markdown
-->
