# State and Everything: A minimalistic Example

I have been told by the nice people of the Idris community
on Discord that every functional web framework has some clickable
counter as its first example.

If that's true, I'm not going to be the one to break with
tradition, so here we go.

The application will consist of three buttons and an internal
counter: One button for increasing the counter by one, one button
for decreasing the counter, and one button for resetting the counter to
zero. On every button click, the user interface should be updated
and display the actual count. In addition, we do not want users
to increase or decrease the counter too much, so the corresponding
buttons should be disabled if values are getting too large or too small.
Resetting the state makes no sense when the counter is at zero,
so the *reset* button should be disabled in that case as well.


```idris
module Examples.Reset

import Examples.CSS.Reset
import Examples.Util
import Web.MVC

%default total
```

## Model

Our model is still too primitive to require a custom
data type. Since the button clicks update an integer value,
our event type will be a function on integers:

```idris
public export
data ResetEv : Type where
  ResetInit : ResetEv
  Mod       : (Int8 -> Int8) -> ResetEv
```

## View

First, we define some custom CSS rules for the
elements specific to this application. These can
be found [here](CSS/Reset.idr).

Next, we need to identify the dynamic components
of our application whose behavior or appearance will
change depending on the current state.
There are four of them: The buttons for increasing and decreasing
the counter, which will be disabled if the value gets to
small or too big, the reset button, which will be disabled
if the counter is at zero, and the div element where we will output
the current count. Again, these have been put to
external module `Examples.CSS.Reset`, because they are
also needed for the CSS rules, and these would
just unnecessarily clutter the code here. Still,
if this is all new to you, make sure to have a quick
look before you continue.

The DOM elements will be laid out as a list of
four lines, each with a descriptive label at the
beginning, followed by the active components.
This is used in many of the example applications,
so it has been moved to its own [utility module](Util.idr).

The three buttons all display some descriptive
text and need to know about the event they fire
when they are being clicked:

```idris
btn : Ref Tag.Button -> (Int8 -> Int8) -> String -> Node ResetEv
btn r ev l = button [Id r, onClick (Mod ev), classes [widget,btn]] [Text l]
```

Finally, we can put the components together and define
the overall application layout:

```idris
content : Node ResetEv
content =
  div [ class resetContent ]
      [ lbl "Reset counter:"    resetLbl, btn btnReset (const 0) "Reset"
      , lbl "Increase counter:" incLbl,   btn btnInc   (+ 1)     "+"
      , lbl "Decrease counter:" decLbl,   btn btnDec   (+ (-1))  "-"
      , lbl "Count:"            countLbl, div [Id out] []
      ]
```

## Controller

The actual controlling MSF is a simple state accumulator, the
output of which will be broadcast to the different dynamic
elements. We use the `fan_` combinator to broadcast
some input to several data sinks (a *sink* is a monadic stream function
that produces no output of interest):

```idris
adjST : ResetEv -> Int8 -> Int8
adjST ResetInit n = 0
adjST (Mod f) n = f n

displayST : Int8 -> List (DOMUpdate ResetEv)
displayST n =
  [ disabled btnDec   (n <= -10)
  , disabled btnInc   (n >= 10)
  , disabled btnReset (n == 0)
  , show out n
  ]

display : ResetEv -> Int8 -> List (DOMUpdate ResetEv)
display ResetInit n = child exampleDiv content :: displayST n
display (Mod f)   n = displayST n

export
runReset : Handler ResetEv => Controller Int8 ResetEv
runReset = runDOM adjST display
```

In the code above, `(>>>)` is sequencing of stream functions
`(f ^>> g)` is an alias for `arr f >>> g`, that allows us to use pure
functions in a sequence of
computations directly, and `accumulateWith` is one of the looping
combinators defined for monadic stream functions. The implementation
of these combinators is pretty simple, so I suggest you have a look
at the code and tutorials in the *rhone* library to get a better understanding
of what's going on under the hood. The data sinks `text` and `disabled`
are defined in module `Rhone.JS.Sink`. They merely use `arrM` to
lift the corresponding effectful computations from the idris2-dom
library to the MSF context.

Finally, we put everything together in a single effectful
computation: Setting up the HTML
content, registering all necessary event listeners,
and returning the stream function.

Note that the function above returns a pair of values: The
value of type `JSIO ()` is a cleanup hook invoked if
this application should be stopped. We return a dummy here:
Every sample application will clear the content of `exampleDiv`,
and we didn't setup any additional resources.

## Some Background: Running Monadic Stream Functions

We will now have a closer look at how the machinery in the background
operates. The first piece of functionality for understanding what's going
on comes in form of function `Data.MSF.Running.step`: This is a single step
evaluation function for MSFs: An MSF is passed an input value, and the result
will be an effectful computation of an output value together with
a new MSF, which will be used in the next evaluation step.

All of this is set up by invoking one of the `reactimateXY` functions
from `Data.MSF.Running`. Go ahead and have a look at their implementations:
I tried to properly annotate the code to make it easier to understand
what's going on: An event handler is being setup and registered at all
active components (this happens, when `innerHtmlAt` is executed), which
will read the current `MSF` (holding the current application state!)
from a mutable variable
and evaluate it using `Data.MSF.Running.step`, whenever an event is fired.
The resulting continuation is then written back to the mutable
variable.

When you look again at the [selector implementation](Selector.md), you
will see that the MSF we defined there will invoke `reactimateIni`
on our `ui` function whenever the user selected the `"reset"` application.

Why the call to `reactimateIni`? When you have a closer look at
the structure of `content` above, you will note that the initial output
field shows no text, and the *reset* button is enabled (the `disabled`
attribute has not been set explicitly). This is not what we want: The current
application state (the value 0) should be correctly shown and the
*reset* button disabled accordingly. We could set these things up
manually in `content`, but that would be a repetition of application
logic. It's often better to setup everything by invoking the `MSF`
once with an *initialization event*. Then everything will behave
correctly from the very beginning.

<!-- vi: filetype=idris2:syntax=markdown
-->
