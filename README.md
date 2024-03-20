# idris2-dom-mvc: Single Page Web Applications in Idris

This is an experimental library about putting a nice API on top
of [idris2-dom](https://github.com/stefan-hoeck/idris2-dom)
for writing interactive single page web applications.
Unlike the [idris2-rhone-js](https://github.com/stefan-hoeck/idris2-rhone-js)
library, which takes a functional reactive programming approach
to GUI programming, the concept of this library is much simpler:
Events fired from user interaction update the current application
state via pure functions, and the UI is updated according to
the current event and new application state. This is a similar
approach to what the [Elm programming language](https://elm-lang.org/)
does. However, we take a more fine-grained approach to updating the DOM
and therefore don't need an extra step via a virtual DOM, which
can be beneficial for performance.

This is still very much work in progress, but I transferred the
whole rhone-js tutorial to this library and the resulting code is
a lot simpler compared to the one from rhone-js.
Here's the link to the [tutorial](docs/src/Examples/Main.md).

## Dependencies

This project makes use of several other Idris2 projects:

* [idris2-elab-util](https://github.com/stefan-hoeck/idris2-elab-util)
* [idris2-dom](https://github.com/stefan-hoeck/idris2-dom)
* [idris2-refined](https://github.com/stefan-hoeck/idris2-refined)
* [idris2-tailrec](https://github.com/stefan-hoeck/idris2-tailrec)
* [idris2-json-simple](https://github.com/stefan-hoeck/idris2-json)

It is strongly suggested to use
a package manager like [pack](https://github.com/stefan-hoeck/idris2-pack)
to install and maintain the required dependencies and build the project.

## Building the Example Page

If you have installed pack as suggested above,
you can build the example page with `make page` and have a look at
it by loading `mvc.html` into your browser.

## Package `dom-mvc-extra`

This additional library contains an opinionated set of utilities I find
generally useful in my own projects. It takes a slightly different
approach towards the "model, view, update" concept, by introducing
stateful computations (from `Control.Monad.State`) paired with optics
(from `idris2-monocle`) for operating on smaller parts of a large
application state.

In addition, it introduces several new data types and interactive DOM
elements that I typically like to have around.
