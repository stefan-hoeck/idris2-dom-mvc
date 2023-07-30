# Making Buttons: A look at Performance

This tutorial's web application comes with the following
ingredients: A validated text input where users are
supposed to enter a positive natural number.
When they hit `Enter` after entering their
number, a corresponding number of buttons is created, each
of which can be clicked exactly once before being disabled,
and the sum of the values clicked will be accumulated and
displayed at the UI. The sum should be reset when a new set
of buttons is created and the input field should be cleared
after the run button was clicked.

Since we want to have a look at the performance of this,
we also include an output field for the time it took to
create and display the buttons.

We are going to iterate over large lists of
items, therefore we need to be conscious about stack space and make
use of the tail-recursive functions.

Here's the list of imports:

```idris
module Examples.Performance

import Data.Either
import Data.List.TR
import Data.Nat
import Data.Refined.Integer
import Data.String

import Derive.Prelude
import Derive.Refined

import Examples.CSS.Performance
import Examples.Util

import Web.MVC
import Web.MVC.Animate

%default total
%language ElabReflection
```

## Model

As before, we first define our event type. We use a refinement type
from the [idris2-refined](https://github.com/stefan-hoeck/idris2-refined)
library for the number of buttons:


```idris

MinBtns, MaxBtns : Integer
MinBtns  = 1
MaxBtns  = 100_000

record NumBtns where
  constructor B
  value : Integer
  {auto 0 prf : FromTo MinBtns MaxBtns value}

%runElab derive "NumBtns" [Show,Eq,Ord,RefinedInteger]

public export
data PerfEv : Type where
  PerfInit   : PerfEv
  NumChanged : Either String NumBtns -> PerfEv
  Reload     : PerfEv
  GotTime    : Integer -> PerfEv
  Clicked    : Nat -> PerfEv
  ClearNum   : PerfEv
```

We also require a function for input validation:

```idris
read : String -> Either String NumBtns
read =
  let err := "expected integer between \{show MinBtns} and \{show MaxBtns}"
   in maybeToEither err . refineNumBtns . cast
```

The application state consists of the currently validated input
plus the current sum.

```idris
public export
record PerfST where
  constructor P
  sum : Nat
  num : Maybe NumBtns

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
    [Id (btnRef n), onClick (Clicked n), classes [widget,btn,inc]]
    [Text $ show n]
```

Next, we write the function to create a grid of buttons.
Since we plan to create thousands of buttons at once, we must
make sure to do this in a stack-safe manner.
Some list functions in the standard libraries are not (yet)
stack safe, so we use stack-safe `iterateTR`
from the [idris2-tailrec](https://github.com/stefan-hoeck/idris2-tailre)
project.
Luckily, in recent commits of the Idris project,
`map` for `List` *is* stack-safe, so we can use it without further
modification.

```idris
btns : NumBtns -> Node PerfEv
btns (B n) = div [class grid] $ map btn (iterateTR (cast n) (+1) 1)
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

We register two events at the text field: Whenever users enter
some text, the field should fire an event to get the validation
routine started. If the *Enter* key is pressed, the grid of
buttons should be generated. This should also happen if the
*Run* button is clicked.

## Controller

As before, we define several pure functions for updating
the state and the DOM depending on the current event.

Adjusting the application state is very simple:

```idris
export
update : PerfEv -> PerfST -> PerfST
update PerfInit       = const init
update (GotTime _)    = id
update (NumChanged e) = {num := eitherToMaybe e}
update Reload         = {sum := 0}
update (Clicked k)    = {sum $= (+k)}
update ClearNum       = {num := Nothing}
```

Updating the DOM is not much harder. Here it is very useful that we
do not use a virtual DOM: Since we don't recreate the whole view on
every event, we don't have to keep track of the disabled buttons,
nor do we have to redraw thousands of buttons, which would drastically
slow down the user interface.

```idris
dispTime : Maybe NumBtns -> Integer -> String
dispTime Nothing  ms = "\Loaded no buttons in \{show ms} ms."
dispTime (Just 1) ms = "\Loaded one button in \{show ms} ms."
dispTime (Just n) ms = "\Loaded \{show n.value} buttons in \{show ms} ms."

displayST : PerfST -> Cmd PerfEv
displayST s = batch [disabledM btnRun s.num, show out s.sum]

displayEv : PerfEv -> PerfST -> Cmd PerfEv
displayEv PerfInit       _ = child exampleDiv content
displayEv (NumChanged e) _ = validate natIn e
displayEv (Clicked k)    _ = disabled (btnRef k) True
displayEv (GotTime n)    s = text time (dispTime s.num n)
displayEv ClearNum       s = value natIn ""
displayEv Reload         s =
  cmdIfJust s.num (timed GotTime . child buttons . btns) <+> pure ClearNum

export
display : PerfEv -> PerfST -> Cmd PerfEv
display e s = displayEv e s <+> displayST s
```

I'd like to look at several of the commands used above. First, note that `Cmd e`
is a monoid, so we can use semigroup append to combine commands. Function `batch`
allows us to combine a list of commands. Command `validate` sets a custom
validation message at an `<input>` field, depending on whether its `Either String x`
argument is a `Left` or a `Right`.

Function `pure` allows us to synchronously
fire another event. Here, we only want to clear the user input and disable
the *run* button again *after* we loaded a new set of buttons. Since the
state is updated first, we can't clear the `numBtns` field before updating
the DOM. An alternative would be to use a function of the following type
instead, which would give us access to the old and new state at the same time:

```haskell
controller : PerfEv -> PerfST -> (PerfST, Cmd PerfEv)
```

Both are valid options.

<!-- vi: filetype=idris2:syntax=markdown
-->
