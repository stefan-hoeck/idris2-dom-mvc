# The Basic Layout of a rhone-js Web Page

This module defines the HTML structure of the main page and
implements the functionality of the `select` element that is
used to choose and load one of the example applications.

First some imports:

```idris
module Examples.Selector

import Derive.Lens
import Examples.CSS
import Monocle

import public Data.List.Quantifiers.Extra
import public Examples.Balls
import public Examples.Fractals
import public Examples.MathGame
import public Examples.Performance
import public Examples.Reset
import public Web.MVC

%default total
%language ElabReflection
```

This imports all example applications plus module `Examples.CSS`, where
most of the CSS rules for the page are defined. In addition, `Rhone.JS`
is imported, the kitchen sink re-exporting the core functionality necessary
to write a rhone-js wep application.

## ~~The Effect Type: `MonadDom`~~

This library used to have its own effect type called `MonadDom`. However, it
turned out that this made the whole thing unnecessarily complex with adding
any benefit. Nowadays, we can just use plain `JSIO`.

## Writing HTML in Idris2

Module `Text.Html` and its submodules provide a small DSL for
declaring HTML nodes and their attributes. These are pure
Idris data types and can be used to write and render
properly formatted HTML on any backend.

Here is the layout of the main page:

```idris
public export
data App = Reset | Perf | Balls | Fract | Math

toApp : String -> App
toApp "perf"  = Perf
toApp "balls" = Balls
toApp "fract" = Fract
toApp "math"  = Math
toApp _       = Reset

content : Node App
content =
  div [ class contentList ]
      [ div [class pageTitle] ["dom-mvc: Examples"]
      , div [class contentHeader]
          [ label [class widgetLabel] ["Choose an Example"]
          , select
              [ classes [widget, selectIn, exampleSelector], onChange toApp]
              [ option [ value "reset", selected True ] ["Counting Clicks"]
              , option [ value "performance" ] ["Performance"]
              , option [ value "fractals" ] ["Fractals"]
              , option [ value "balls" ] ["Bouncing Balls"]
              , option [ value "math" ] ["Math Game"]
              ]
          ]
      , div [Id exampleDiv] []
      ]
```

A typical `Node` constructor like `div` or `label` takes
two arguments: A list of attributes and a list of child
nodes.

Several things need some quick explanation: CSS classes like
`pageTitle` or `contentHeader` are just `String`s defined in
module `Examples.CSS` together with the corresponding CSS rules.
(If you are new to web development: [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS)
is a domain specific
language used to describe the presentation of documents written
in HTML and similar markup languages. It is used to define the
appearance of the example web page.)

DOM identifiers like `exampleDiv` are of type `ElementRef t` (defined
in `Rhone.JS.ElemRef`), where `t` is the type of the
corresponding HTML element. They are mainly typed wrappers around ID
strings and are used to lookup HTML elements in the DOM.
We need these, whenever an element in the DOM is not static.
In the example above, `exampleDiv` points to the DOM element
where the content of the example applications will go. It's the
only part of the main web page that is not static.

Finally, we also encode the events an element fires in the `Node`
type, and that's what `Node`'s parameter stands for. Events are
just attributes, and in the example above, the `select` element
fires an event whenever the user changes the selected value
(`onChange id`).

## The Interactive Part: Monadic Stream Functions

Now that we have the structure of our web page specified, we
can have a quick look at how we define its interactive behavior.

rhone-js is named after [idris2-rhone](https://github.com/stefan-hoeck/idris2-rhone)
a port of monadic stream functions (MSF) first introduced
in Haskell's [dunai](https://hackage.haskell.org/package/dunai)
library and explained in detail in a nice
[article](https://www.cs.nott.ac.uk/~psxip1/#FRPRefactored).
This is probably the most accessible implementation of (arrowized)
functional reactive programming I have so far come across.

In its most general form, an MSF can be thought of
as having the following type:

```haskell
data MSF m i o = MSF (i -> m (o, MSF m i o))
```

This is an effectful computation over a monad type `m` converting
an input value of type `i` to an output value of type `o` plus
a new MSF (the original MSF's continuation), which will be used
to convert the next piece of input.

In the *rhone* library, we don't use this most general form,
as it does not go well with the totality checker. Instead,
our implementation of `MSF` encodes a rather large set
of primitive operations in the data type itself. This gives
us the benefit of provably total stream functions, which is
extremely valuable especially when we start using advanced
components like event switches.

`MSF` implements interfaces `Functor` and `Applicative` but also
`Category` and `Arrow`. It lets us lift any effectful function
into an `MSF` value via function `arrM` (used in the code below),
supports feedback loops for stateful computations and several
kinds of event switches to dynamically change the behavior
of a web page. The ability to lift arbitrary effectful computations
is especially useful, as it allows us to update the DOM
directly from within an MSF.

Enough talk, here's the code:

```idris
public export
0 Events : List Type
Events = [App, BallsEv, FractEv, PerfEv, ResetEv, MathEv]

public export
0 SelectEv : Type
SelectEv = HSum Events

public export
record ST where
  constructor S
  perf  : PerfST
  reset : Int8
  balls : BallsST
  fract : FractST
  math  : MathST

%runElab derive "ST" [Lenses]

export
init : ST
init = S init 0 init init init

0 Controller : Type -> Type
Controller e = e -> ST -> JSIO ST

runSelect : Handler SelectEv -> App -> JSIO ST
runSelect h Perf  = modifyA perfL  (runPerf h Init) init
runSelect h Balls = modifyA ballsL (runBalls h Init) init
runSelect h Fract = modifyA fractL (runFract h Init) init
runSelect h Math  = modifyA mathL  (runMath h Init) init
runSelect h Reset = do
  updateDOM (h . inject)
    [ style appStyle allRules , child contentDiv content ]
  modifyA resetL (runReset h Init) init

cleanup : ST -> JSIO ()
cleanup s = liftIO (s.balls.cleanUp >> s.fract.cleanUp)

controllers : (SelectEv -> JSIO ()) -> All Controller Events
controllers h =
  [ \e,s => cleanup s >> runSelect h e
  , modifyA ballsL . runBalls h
  , modifyA fractL . runFract h
  , modifyA perfL  . runPerf h
  , modifyA resetL . runReset h
  , modifyA mathL  . runMath h
  ]

export
ui : (SelectEv -> JSIO ()) -> SelectEv -> ST -> JSIO ST
ui h es s = collapse' $ hzipWith (\f,e => f e s) (controllers h) es
```

I'll quickly break this down a bit: The first line
renders and applies the page's CSS rules to a `<style>`
element in the HTML header referenced by `ElementRef`
`appStyle`. The second line,
`innerHtmlAt contentDiv content`, is where half of the magic
happens: We change the inner HTML of the element with ID
`"content"` (the string wrapped up in `contentDiv`) to the
result of rendering `content` (the HTML `Node` we defined above).
At the same time, event listeners are attached to all interactive
elements of our web page.

Afterwards, the monadic stream function is created: We
use `arrM` to lift an effectful computation to the MSF context.
This is just a pattern match on our event type (`String`), which consists
of the values fired by the select element. If the value is one we
know about, we start the corresponding user interface, again
by invoking the mysterious `reactimateInDom` function. This
is where the other half of the magic happens, but that's for
another post.

Just a final note: `reactimate` and `reactimateIni`
might set up resources (for instance running timers) we need to cleanup
once we switch to another example application.
The `feedback` loop takes care of this:
After a new example has been started, its cleanup hook is
sent back to the input of the controlling MSF and invoked
before the next application is started.

## Comparison with other MVC Libraries

Frameworks for writing interactive web applications often
use the term *MVC* (model, view, controller), where
*model* refers to the underlying data model (or state)
of the web page, *view* is the visualization of the model
as a collection of DOM elements, and *controller* is the part where
the logic of the web application lies: It is responsible
for updating the model (state) due to user interactions
and refresh the web page accordingly.

Several MVC implementations,
for instance the one used by the *Elm* programming language,
make use of a *virtual DOM*. This is an in-memory model of
the real DOM used by the browser, and this is the *view*
that is being manipulated in Elm applications. On each
update, the virtual DOM is compared to its previous state
(a process called *DOM diffing*) and the real DOM is updated
to reflect the changes made to the virtual DOM. The advantage of this
approach is that we can write a single (pure!) function for converting
the model to the view and never have to interact with
the real DOM explicitly. The downside is, that we loose some
control over which parts of the web page are updated when,
which can have an impact on performance, especially when
the web page - and thus the virtual DOM - consists of
many elements.

So far, rhone-js does not use a virtual DOM but interacts with
the real DOM directly through a network of monadic stream
functions. Whether this will result in a nice way to write web applications
or will lead to unmaintainable tangles of code, only time
and experience will tell.

## Whats next?

In the [next part](Reset.md), I'll explain a first sample application
with some real application state and reactive components in detail.
Have fun!

<!-- vi: filetype=idris2:syntax=markdown
-->
