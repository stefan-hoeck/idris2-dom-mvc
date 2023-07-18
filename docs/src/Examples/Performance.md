# Making Buttons: A look at Performance

This tutorial's web application comes with the following
ingredients: A validated text input where users are
supposed to enter a positive natural number.
When they hit `Enter` after entering their
number, a corresponding number of buttons is created, each
of which can be clicked exactly once before being disabled,
and the sum of the values clicked will be accumulated and
displayed at the UI. The sum should be reset when a new set
of buttons is created.

Since we want to have a look at the performance of this,
we also include an output field for the time it took to
create and display the buttons.

We are going to iterate over large lists of
items, therefore we need to be conscious about stack space and make
use of the tail-recursive functions.
We also write our first event data type, for which we will
automatically derive certain interfaces using elaborator
reflection. The corresponding functionality comes from
the [idris2-elab-util](https://github.com/stefan-hoeck/idris2-elab-util)
library.

Here's the list of imports:

```idris
module Examples.Performance

import Data.DPair
import Data.Either
import Data.List.Quantifiers.Extra
import Data.List.TR
import Data.Nat
import Data.String

import Examples.CSS.Performance
import Examples.Util

import Web.MVC
import Web.MVC.Animate

%default total
```

## Model

Since the buttons in the button grid have no effect
on the behavior of the components responsible for
creating the button grid, we can completely decouple
the two parts. This is similar to what we did for
the [example selector](Selector.md).

This means we have two unrelated models, the first being just
natural numbers that are summed up, the second being
the events from the text input. For the latter, we use
a custom data type:

```idris
public export
0 PosNat : Type
PosNat = Subset Nat IsSucc

public export
data PerfEv : Type where
  PerfInit   : PerfEv
  NumChanged : Either String PosNat -> PerfEv
  Reload     : PerfEv
  Set        : Nat -> PerfEv
```

We also require a function for input validation:

```idris
read : String -> Either String PosNat
read s = case cast {to = Nat} s of
  Z       => Left "Not a positive natural number: \{s}"
  n@(S _) => Right $ Element n ItIsSucc

public export
record PerfST where
  constructor P
  sum : Nat
  num : Maybe PosNat

export
init : PerfST
init = P 0 Nothing
```

## View

The CSS rules and reference IDs have again been moved
to their [own module](CSS/Performance.idr), to declutter
the code here. We also use labeled lines of input elements
as in the [previous example](Reset.idr). For the
grid of buttons, we need a reference for each button,
since we want to disable them after they have been clicked:

```idris
btnRef : Nat -> Ref Tag.Button
btnRef n = Id "BTN\{show n}"

btn : Nat -> Node PerfEv
btn n =
  button
    [Id (btnRef n), onClick (Set n), classes [widget,btn,inc]]
    [Text $ show n]
```

Next, we write the function to create a grid of buttons.
Since we plan to create thousands of buttons at once, we must
make sure to do this in a stack-safe manner.
Some list functions in the standard libraries are not (yet)
stack safe, so we define our own stack-safe `iterateTR` function here.
In this case, the recursive calls are in tail position,
so the JS backends can convert the function to a while loop using
constant stack space. Luckily, in recent commits of the Idris project,
`map` for `List` *is* stack-safe, so we can use it without further
modification.

```idris
btns : PosNat -> Node PerfEv
btns n = div [class grid] . map btn $ iterateTR (fst n) (+1) 1
```

And, finally, the overall layout of the application:

```idris
content : Node PerfEv
content =
  div [ class performanceContent ]
    [ lbl "Number of buttons:" numButtonsLbl
    , input [ Id natIn
            , onInput (NumChanged . read)
            , onEnterDown Reload
            , classes [widget, textIn]
            , placeholder "Enter a positive integer"
            ] []
    , button [Id btnRun, onClick Reload, classes [widget, btn]] ["Run"]
    , lbl "Sum:" sumLbl
    , div [Id out] []
    , div [Id time] []
    , div [Id buttons] []
    ]
```

We register two events at the text field: Whenever users input
text in it, the field should fire an event to get the validation
routine started. If the *Enter* key is pressed, the grid of
buttons should be generated. This should also happen if the
*Run* button is clicked.

## Controller

We have to implement two controllers: One for calculating the
sum of buttons clicked (and disabling the clicked buttons),
and one for validating user input and setting up new grids of
buttons.

We also need a way to calculate the time taken to create
and display the buttons. The idris2-dom library does not
yet provide this functionality, but it is available
from `Rhone.JS.Util`:

```idris
dispTime : PosNat -> Integer -> String
dispTime (Element 1 _) ms = "\Loaded one button in \{show ms} ms."
dispTime (Element n _) ms = "\Loaded \{show n} buttons in \{show ms} ms."
```

The reactive behavior of the grid of buttons consists of
two pieces of functionality: The first is used to accumulate
the sum of values and print the result to the output div,
the second is responsible for disabling every button that
has been clicked. However, trying to disable a non-existing
button will lead to an error message being printed to the
console. Since we plan to initialize the signal function
with an input of zero, we must make sure the disabling
part is only invoked on non-zero input.
The call to `ifIsNot 0` makes sure that the following
stream function is only evaluated if the input is indeed
a positive natural number. Again, have a look at this
function's implementation in the *rhone* library. There
are many similar combinator and they are very
convenient to use.

```idris
adjST : PerfEv -> PerfST -> PerfST
adjST PerfInit       = const init
adjST (NumChanged e) = {num := eitherToMaybe e}
adjST Reload         = {sum := 0}
adjST (Set k)        = {sum $= (+k)}

displayST : PerfST -> List (DOMUpdate PerfEv)
displayST s = [disabledM btnRun s.num, show out s.sum]

displayEv : PerfEv -> PerfST -> DOMUpdate PerfEv
displayEv PerfInit       _ = child exampleDiv content
displayEv (NumChanged e) _ = validate natIn e
displayEv (Set k)        _ = disabled (btnRef k) True
displayEv Reload         s = maybe NoAction (child buttons . btns) s.num

display : PerfEv -> PerfST -> List (DOMUpdate PerfEv)
display e s = displayEv e s :: displayST s

export
runPerf : Has PerfEv es => SHandler es -> PerfEv -> PerfST -> JSIO PerfST
runPerf h e s = do
  (s2,dt) <- timed (injectDOM adjST display h e s)
  case (e,s2.num) of
    (Reload,Just n) => updateDOM h [text time $ dispTime n dt]
    _               => pure ()
  pure s2
```

Here is the function used to create a non-zero number of
buttons, add them to the UI and display the time taken
to do so:

```idris
-- btnsSF : PosNat -> Handler JSIO Nat -> JSIO (MSF JSIO Nat (), JSIO ())
-- btnsSF n h = do
--   t1 <- currentTime
--   innerHtmlAt buttons (btns n)
--   t2 <- currentTime
--   rawInnerHtmlAt time (dispTime n.fst $ t2 - t1)
--   pure (sumNats, pure ())
```

The second controller takes care of validating user
input (the number of buttons entered in the text
field), and reloading the button grid upon a
`Reload` event:

```idris
-- count : MSF JSIO Ev (Either String PosNat)
-- count =    getInput Validate validate natIn
--        >>> observeWith (isLeft ^>> disabledAt btnRun)
```

Input validation can be quite involved (I wrote a lengthy
tutorial about this for the *rhone* project), but
utility function `getValue` takes care of this for us.
However, we also need to make sure to disable the *Run* button
in case of invalid input.

```idris
-- msf : MSF JSIO Ev ()
-- msf =   fan [count, is Reload]
--     >>> rightOnEvent
--     ?>> arrM (ignore . reactimateIni 0 . btnsSF)
```

The `rightOnEvent` combinator comes up often in user
interfaces: Some input needs to be validated and
processed whenever an appropriate event is fired.
The input stream function holds an `Either a b`, but we need
an `a` to continue. There are of course also combinators
`leftOnEvent`, `justOnEvent`, and `nothingOnEvent`.

```idris
-- export
-- ui : Handler JSIO Ev => JSIO (MSF JSIO Ev (), JSIO ())
-- ui = innerHtmlAt exampleDiv content $> (msf, pure ())
```

<!-- vi: filetype=idris2:syntax=markdown
-->
