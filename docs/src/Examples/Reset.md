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
so the *reset* button should be disabled in that case.


```idris
module Examples.Reset

import Examples.CSS.Reset
import Examples.Util
import Web.MVC

%default total
```

## Model

Our model is still too primitive to require a custom
data type: We just use an `Int8` for the current number.
The event type is not much more complex: We define an
event for initializing the UI, and one for updating
the model:

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
the counter, which will be disabled if the value gets too
small or too big, the reset button, which will be disabled
if the counter is at zero, and the div element where we will output
the current count. Again, the corresponding `Ref t`s
have been put to external module `Examples.CSS.Reset`, because they are
also needed for the CSS rules.
If this is all new to you, make sure to have a quick
look at how they are defined before you continue.

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
  div
    [ class resetContent ]
    [ lbl "Reset counter:"    resetLbl, btn btnReset (const 0) "Reset"
    , lbl "Increase counter:" incLbl,   btn btnInc   (+ 1)     "+"
    , lbl "Decrease counter:" decLbl,   btn btnDec   (+ (-1))  "-"
    , lbl "Count:"            countLbl, div [Id out] []
    ]
```

Node, that unlike in the Elm Architecture, we did not display the
whole model in `content`, although it would certainly have been
possible to do so. This is a matter of preference and sometimes a
matter of performance.

Personally, I prefer having a separate function for updating
the dynamic parts of the view, depending on the current event
and model. This makes the things that change on certain events
stand out somewhat more at the cost of some more verbose code.

## Controller

We typically define three pure functions for controlling the
application: First, one for adjusting the application state
according to the current event. We call this one `update` as
in Elm:

```idris
export
update : ResetEv -> Int8 -> Int8
update ResetInit n = 0
update (Mod f) n = f n
```

Second, one for displaying the current state and possibly causing
other side effects. This lists - amongst other things - the necessary
updates to the DOM. Some of these effects might setup new event sources,
by setting up new components in the DOM, starting an animation, or sending
a HTTP request. Therefore, the return type is not `IO` or `JSIO` but
`Cmd`. I tend to call these functions `display`, although the can do
more than just displaying stuff as we will see in later parts of the
tutorial.

Here is a function that always updates the view according to
the current state. It will define whether the buttons are enabled or not,
and it will print the state of the counter:

```idris
displayST : Int8 -> Cmd ResetEv
displayST n =
  batch
    [ disabled btnDec   (n <= -10)
    , disabled btnInc   (n >= 10)
    , disabled btnReset (n == 0)
    , show out n
    ]
```

And here is the main `display` function for acting on the
current event and updated state (the main controller `runMVC`
we use in our main application always updates the state first,
before using it to compute the command):

```idris
export
display : ResetEv -> Int8 -> Cmd ResetEv
display ResetInit n = child exampleDiv content <+> displayST n
display (Mod f)   n = displayST n
```

All the `Cmd` actions used above are defined in module
`Web.MVC.View`. It is pretty straight forward to define your
own `Cmd` in case some functionality is missing.
A `Cmd e` is a wrapper around `(handler : e -> JSIO ()) -> JSIO ()`, where
the `handler` argument can be used as an event listener and invoked
for firing new events synchronously or asynchronously.

Here is a non-comprehensive list of predefined `Cmd`s:

* `children`: Replaces a node's child nodes with a new list of nodes.
* `child`: Replaces a node's child nodes with a single new node.
* `text`: Replaces a node's child nodes with a text node.
* `show`: Like `text` but uses `show` on its argument to create a string
  to display.
* `raw`: Replaces a node's child nodes with new nodes generated from the
  string argument containing raw HTML.
* `style`: Replaces a `<style>` node's content if a set of new CSS rules.
* `append`: Appends a node to the list of children of another node.
* `prepend`: Prepends a node to the list of children of another node.
* `before`: Puts a node before another in a list of child nodes.
* `after`: Puts a node after another in a list child nodes.
* `replace`: Replace a whole node with another one.
* `remove` : Removes a node and all its children from the DOM.
* `validityMsg`: Set a custom validity message at an element.
* `validate`: Set a custom validity message at an element based
  on an `Either String a`.
* `attr`: Sets a single attribute at a node. This could also be a new
  event.
* `render` : Render a `Scene` at a canvas element.
* `value` : Change the value of an `<input>` or similar element.
* `focus` : Focus the given element. Useful when a new `<input>` field
  or button has just been created.

As we will see in later parts of the tutorial, it is quite straight forward
to create ones own `Cmd e` values.

<!-- vi: filetype=idris2:syntax=markdown
-->
