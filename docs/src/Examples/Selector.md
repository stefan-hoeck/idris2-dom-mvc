# The Basic Layout of a dom-mvc Web Page

This module defines the HTML structure of the main page and
implements the functionality of the `<select>` element that is
used to choose and load one of the example applications.

First some imports:

```idris
module Examples.Selector

import Derive.Lens
import Derive.Finite
import Examples.CSS
import Examples.CSS.Core
import Monocle
import Text.HTML.Select

import public Data.List.Quantifiers.Extra
import public Examples.Balls
import public Examples.Fractals
import public Examples.MathGame
import public Examples.Performance
import public Examples.Requests
import public Examples.Reset
import public Web.MVC

%default total
%language ElabReflection
```

This imports all example applications plus module `Examples.CSS`, where
most of the CSS rules for the page are defined. In addition, `Web.MVC`
is imported, the kitchen sink re-exporting the core functionality necessary
to write a single page web application.

Some stuff from external libraries is also imported:

* `Monocle`: From the [idris2-monocle](https://github.com/stefan-hoeck/idris2-monocle)
   library, an optics library for derivable setters and getters for deeply
   nested data structures. We use it to effectfully update the fields
   of our main application state.
* `Derive.Lens`: Also from the monocle library, this allows us to derive
  lenses for the record type we use as the main application state.
* `Derive.Finite`: This is from the
  [idris2-finite](https://github.com/stefan-hoeck/idris2-finite)
  library, which allows us to derive and enumerate all values of a type with
  a finite number of inhabitants.
* `Data.List.Quatifiers.Extra`:
  This is from the
  [idris2-quantifiers-extra](https://github.com/stefan-hoeck/idris2-quantifiers-extra)
  library, providing some additional functionality for working with heterogeneous
  lists and sums. We use a heterogeneous sum to collect the events our
  different example applications fire.

## Writing HTML in Idris2

Module `Text.HTML` and its submodules provide a small
domain-specific language (DSL) for
declaring HTML nodes and their attributes. These are pure
Idris data types and can be used to write and render
properly formatted HTML on any backend.

In order to write an interactive web page, we first need to define
the type of events it fires. The `<select>` element at the top of the
page is used to select one of several example applications. We list
the possible values in an enumeration and provide a function for
converting the string inputs from the user interface to the
correct event value:

```idris
public export
data App = Reset | Perf | Req | Balls | Fract | Math

%runElab derive "App" [Show,Eq,Finite]

toApp : String -> App
toApp s = fromMaybe Reset $ find ((s ==) . show) values
```

Here is the layout of the main page:

```idris
appName : App -> String
appName Reset = "Counting Clicks"
appName Req   = "Processing HTTP Requests"
appName Perf  = "Performance"
appName Balls = "Bouncing Balls"
appName Fract = "Fractals"
appName Math  = "Math Game"

content : App -> Node App
content ini =
  div [ class contentList ]
      [ div [class pageTitle] ["dom-mvc: Examples"]
      , div [class contentHeader]
          [ label [class widgetLabel] ["Choose an Example"]
          , selectFromList values (Just ini) appName id
              [classes [widget, selectIn, exampleSelector], onChange toApp]
          ]
      , div [Id exampleDiv] []
      ]
```

A typical `Node` constructor like `div` or `label` takes
two arguments: A list of attributes and a list of child
nodes. We use these to describe the tree structure of a HTML page.
This works very well, as the code is typically quite readable, while
we still have the power of Idris at hand: The options of the select
element come from applying function `opt` to all `App` values.
(Function `values` comes from interface `Data.Finite.Finite` from the
finite library.)

Several things need some quick explanation: CSS classes like
`pageTitle` or `contentHeader` are just `String`s defined in
module `Examples.CSS` together with the corresponding CSS rules.
(If you are new to web development:
[CSS](https://developer.mozilla.org/en-US/docs/Web/CSS)
is a domain specific
language used to describe the presentation of documents written
in HTML and similar markup languages. It is used to define the
appearance of the example web page.)
In this example application I show how to define CSS rules in Idris
using the data types and functions from `Text.CSS` and its submodules.
If you prefer to use plain `.css` files instead (as I do nowadays),
that's perfectly fine as well.

DOM identifiers like `exampleDiv` are of type `Ref t` (defined
in `Text.HTML.Ref`), where `t` is a tag of type `Text.HTML.Tag.HTMLTag`
corresponding to the HTML element's tag. They are mainly typed wrappers
around ID strings and are used to lookup HTML elements in the DOM.
Tag `t` gives us some additional guarantees, both when building
a `Node` (the element and ID tag must match), but also when looking
up elements in the DOM, or when we need to restrict the allowed
elements in a function.
In the example above, `exampleDiv` points to the DOM element
where the content of the example applications will go. It's the
only part of the main web page that is not static.

Finally, we also encode the events an element fires in the `Node`
type, and that's what `Node`'s parameter stands for. Events are
just attributes, and in the example above, the `select` element
fires an `App` event whenever the user changes the selected value
(`onChange toApp`).

## The Interactive Part: Handling Events

Now that we have the structure of our web page specified, we
can have a quick look at how we define its interactive behavior.

The core function used for this (which is used in our application's
`main` function) is `Web.MVC.runController`. Here's its type

```haskell
runController :
     {0 e,s  : Type}
  -> (initEv : e)
  -> (initST : s)
  -> (ctrl   : Handler e => Controller s e)
  -> JSIO ()
```

We are going to describe the different parts in detail: An interactive
web application fires and handles events from user input. In the type
above, `e` is the event type, and `initEv` is the initial event
we fire when starting the application.

Non-trivial web pages are stateful: An event modifies the application
state (for instance, by increasing a counter when a button is clicked)
and the new state is used to update what we see in the user interface.
The state type in `runController` is represented by parameter `s`, and `initST`
is the initial application state.

Finally, we need a way to update and display the state when an event
occurs, and this is what function `ctrl` does. Now, this is the
most complex argument, so we need to talk about it in some detail.
The first argument of `ctrl` is of type `Handler e`, which is an
wrapper around `e -> JSIO ()`. So `ctrl` is given a function for handling
events, which it can use to register event listeners at the user interface.
As we will see, we don't typically register event handlers manually,
as the utilities we invoke do this for us. However, they need access
to an event handler to do so, and that's what the first argument of `ctrl`
is used for.

The result type `Controller s e` is an alias for `e -> s -> JSIO s`,
so `ctrl` actually takes two more arguments.
These are the current event and application state, respectively,
and the result is the updated application state plus
some side effects for updating the user interface in accordance
with the fired event and new application state.

As we will see, we often use pure functions together with some
existing utilities for `ctrl`, which will tremendously simplify
our code. Still, for the main application, we are going to need
the components described above.

First, we define the event type. Our main application consists of a selection
of several unrelated example apps, each with its own state and
event type. We collect all event types plus our own `App` in
a heterogeneous sum, and use this as the main event type of the
whole application:

```idris
||| We don't include `App` here, because we need this list in
||| the type of `runApp` below.
public export
0 Events : List Type
Events = [BallsEv, FractEv, PerfEv, ResetEv, MathEv, ReqEv]

||| The full event type includes `App` at the head, so we
||| can split it of with a simple pattern match (see `ui`).
public export
0 FullEv : Type
FullEv = HSum (App :: Events)
```

Likewise, we use a record type listing the states of the different
applications in its fields. We are going to use the lenses from
monocle to modify a single application state, depending on the
input being fired. We also define the initial application state,
which just lists the initial state of each example application.

```idris
public export
record ST where
  constructor S
  perf  : PerfST
  reset : Int8
  balls : BallsST
  fract : FractST
  math  : MathST
  req   : ReqST

%runElab derive "ST" [Lenses]

export
init : ST
init = S init 0 init init init RS
```

We can now implement the main application controller. In order to
distinguish between the events coming from each application,
we use a heterogeneous list holding one controller for each event type.
Function `Web.MVC.controlMany` is useful for this. All controllers
are going to need access to the main event handler, so we pass it via
a `parameters` block:

```idris
parameters {auto h : Handler FullEv}

  runApp : Controller ST (HSum Events)
  runApp =
    controlMany
      [ modifyA ballsL . runBalls @{inject h}
      , modifyA fractL . runFract @{inject h}
      , modifyA perfL  . runPerf  @{inject h}
      , modifyA resetL . runReset @{inject h}
      , modifyA mathL  . runMath  @{inject h}
      , modifyA reqL   . runReq   @{inject h}
      ]
```

Function `runApp` only handles the events from example applications
but not the `App` event from the main `<select>` element. We handle
that one in two functions: First, when we change the example application,
we have to cleanup some stuff first: Some applications use animations,
and we'd like to stop those before starting a new app. Cleanup
hooks are stored in the corresponding application states.

We also reset the main view, to make sure all traces from previous
applications are properly removed:

```idris
  cleanup : App -> ST -> JSIO ST
  cleanup x s = do
    liftIO (s.balls.cleanUp >> s.fract.cleanUp)
    putStrLn "Changing app to \{show x}"
    updateDOM @{inject h} [style appStyle rules, child contentDiv $ content x]
    pure init
```

In a second step, we choose the example app to start by pattern
matching on the `App` event fired by the `<select>` element:

```idris
  changeApp : Controller ST App
  changeApp Perf  = runApp (inject PerfInit)
  changeApp Balls = runApp (inject BallsInit)
  changeApp Fract = runApp (inject FractInit)
  changeApp Math  = runApp (inject MathInit)
  changeApp Reset = runApp (inject ResetInit)
  changeApp Req   = runApp (inject ReqInit)
```

As we will see when we look at each of the example applications, every
one of them sets up its own HTML nodes upon receiving its `Init` event.

To sum it all up, and to define function `ui`, which is directly invoked
by function `main`, we just pattern match on the event we get. If it's
an `App` signalling that the user wants to try a different example
app, we cleanup and switch applications. If it's coming from a running
application, however, we just pass it on to `runApp`.

```idris
  export
  ui : Controller ST FullEv
  ui (Here x)  s = cleanup x s >>= changeApp x
  ui (There x) s = runApp x s
```

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
event, the model (state) is updated based on the current event,
the view (virtual DOM) is updated to display the new state, and
the virtual DOM is compared to its previous version
(a process called *DOM diffing*) and the real DOM is updated
to reflect the changes made to the virtual DOM. The advantage of this
approach is that we can often write pretty simple and pure functions
for updating the model and converting
the model to the view and never have to interact with
the real DOM explicitly. The downside is, that we loose some
control over which parts of the web page are updated when,
which can have an impact on performance, especially when
the web page - and thus the virtual DOM - consists of
many elements.

So far, dom-mvc does not use a virtual DOM but uses both
the current event and updated state to determine the parts of
the DOM that need modification.

## Whats next?

In the [next part](Reset.md), I'll explain a first sample application
with some real application state and reactive components in detail.
Have fun!

<!-- vi: filetype=idris2:syntax=markdown
-->
