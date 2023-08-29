# The Basic Layout of a dom-mvc Web Page

This module defines the HTML structure of the main page and
implements the functionality of the `<select>` element that is
used to choose and load one of the example applications.

First some imports:

```idris
module Examples.Selector

import Derive.Finite

import Examples.CSS
import Examples.CSS.Core
import Examples.Balls
import Examples.Fractals
import Examples.MathGame
import Examples.Performance
import Examples.Requests
import Examples.Reset

import Text.HTML.Select
import Web.MVC

%default total
%language ElabReflection
```

This imports all example applications plus module `Examples.CSS`, where
most of the CSS rules for the page are defined. In addition, `Web.MVC`
is imported, the kitchen sink re-exporting the core functionality necessary
to write a single page web application.

Some stuff from external libraries is also imported:

* `Derive.Finite`: This is from the
  [idris2-finite](https://github.com/stefan-hoeck/idris2-finite)
  library, which allows us to derive and enumerate all values of a type with
  a finite number of inhabitants.

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
              [classes [widget, selectIn, exampleSelector]]
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
fires an `App` event whenever the user changes the selected value.

## The Interactive Part: Handling Events

Now that we have the general structure of our web page specified, we
can have a quick look at how we define its interactive behavior.

The core function used for this (which is used in our application's
`main` function) is `Web.MVC.runController`. Here's its type

```haskell
runController :
     {0 e,s  : Type}
  -> (ctrl   : e -> s -> (s, Cmd e))
  -> (onErr  : JSErr -> IO ())
  -> (initEv : e)
  -> (initST : s)
  -> IO ()
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
Its two arguments are the current event and application state,
respectively, and the result is the updated application state plus
a value of type `Cmd e`, an abbreviation for "command".

Type `Cmd e` is defined as follows:

```haskell
public export
record Cmd (e : Type) where
  constructor C
  run : (handler : e -> JSIO ()) -> JSIO ()
```

Given an argument *handler* of type `e -> JSIO ()` it runs some effectfull
computation. We use commands for all the side effect that occur when
a user interacts with our web page. The handler is used as the event
listener to be installed at new interactive components, for instance.
So, a command is not only used to display stuff at the UI but also
to set up new event sources. Here are a couple of use cases for
commands, some of which we will look at in the example applications:

* Add or replace whole parts of the user interface dynamically, which
  will then fire their own events.
* Send and receive a HTTP request to fetch data from another server.
  Once the data is ready, a new event will have to be fired
* Set up a timer to produce an event at regular intervals.
* Set up an animation, to redraw a canvas element
  as quickly and smoothly as possible.
* Run an effectful computation such as a random number generator to
  fire a new event immediately.

As you can see, commands keep the user interface dynamic and interactive.
Now, the type of function `ctrl` encapsulates everything we need to get
going. However, it is not always the most convenient way to react
on events. In many applications the code will get nicer and more
readable if we split up the two steps united in `ctrl`: Updating the
state and producing a new command. This is what we will be doing throughout
the example applications.

### Defining the Event Type

Our main application consists of a selection
of several completely unrelated example apps, each with its own state and
event type. We collect all event types plus our own `App` event in
a heterogeneous sum, and use this as the main event type of the
whole application. For initializing the whole thing, we add an event
called `Init`:

```idris
public export
data Event : Type where
  Init      : Event
  ChangeApp : App -> Event
  EBalls    : BallsEv -> Event
  EFract    : FractEv -> Event
  EPerf     : PerfEv  -> Event
  EReset    : ResetEv -> Event
  EMath     : MathEv  -> Event
  EReq      : ReqEv   -> Event
```

### Defining the Application State

Likewise, we use a record type listing the states of the different
applications in its fields.
We also define the initial application state,
which just lists the initial state of each example application.

```idris
public export
record ST where
  constructor S
  perf    : PerfST
  reset   : Int8
  balls   : BallsST
  fract   : FractST
  math    : MathST

export
init : ST
init = S init 0 init init init
```

### Updating the State

Updating the state of the main application is very straight forward:
We just update each application's state when it fires one of its events:

```idris
update : Event -> ST -> ST
update Init          = id
update (ChangeApp x) = id
update (EBalls x)    = {balls $= update x}
update (EFract x)    = {fract $= update x}
update (EPerf x)     = {perf  $= update x}
update (EReset x)    = {reset $= update x}
update (EMath x)     = {math  $= update x}
update (EReq x)      = id
```

Note how this allows us to combine completely unrelated applications
and run one at a time or several at once if we wanted to.

### Issuing Commands

The function for issuing the next commands based on the current event
and state is a bit more involved. First, some applications run animations
or timers, and we must make sure to stop those when our users would like
to switch applications. The two applications in question provide their
own cleanup hooks in their state, so we invoke those to make sure all
animations and timers are stopped:

```idris
cleanup : ST -> IO ()
cleanup s = s.balls.cleanup >> s.fract.cleanup
```

I usually call the function for issuing commands "display", but other names
would be just as valid. I'll break down the `display` function into several
parts. First, if one of the example applications fires an event, we just
pass it on to the corresponding application's `display` function. Their
events need to be wrapped in our own `Event` type, and luckily, `Cmd` is
a `Functor`, so we can use the `(<$>)` operator (an alias for `map`) to
do just that:

```idris
display : Event -> ST -> Cmd Event
display (EBalls x) s = EBalls <$> display x s.balls
display (EFract x) s = EFract <$> display x s.fract
display (EPerf x)  s = EPerf  <$> display x s.perf
display (EReset x) s = EReset <$> display x s.reset
display (EMath x)  s = EMath  <$> display x s.math
display (EReq x)   s = EReq   <$> display x
```

We must also handle the `Init` event, where the whole user interface
will be set up. Here, we add our CSS rules to pages' `<style>` element,
add the nodes for displaying our `<select>` element as the only child
node of the node represented by `contentDiv`, and we synchronously
fire an event for running the first example application:

```idris
display Init s =
  batch
    [ style appStyle rules
    , ChangeApp <$> child contentDiv (content Reset)
    , pure (ChangeApp Reset)
    ]
```

Finally, we have to deal with the case where a user selects a new
application to run. In this case we must make sure to first run the
cleanup hooks before synchronously fire an initialization event
for the application to run. This application's `display` function
will then receive the event in question (see the cases above) and
initialize things accordingly:

```idris
display (ChangeApp x) s =
  batch
    [ liftIO_ (cleanup s)
    , pure (appInit x)
    ]
  where
    appInit : App -> Event
    appInit Reset = EReset ResetInit
    appInit Perf  = EPerf PerfInit
    appInit Req   = EReq ReqInit
    appInit Balls = EBalls BallsInit
    appInit Fract = EFract FractInit
    appInit Math  = EMath MathInit
```

As we will see when we look at each of the example applications, every
one of them sets up its own HTML nodes upon receiving its `Init` event.

To sum it all up, and to define function `ui`, which is directly invoked
by function `main`, we just pattern match on the event we get. If it's
an `App` signalling that the user wants to try a different example
app, we cleanup and switch applications. If it's coming from a running
application, however, we just pass it on to the application in question.

```idris
export covering
ui : IO ()
ui = runMVC update display (putStrLn . dispErr) Init init
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
