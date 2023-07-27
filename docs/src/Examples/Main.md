# A Guided Tour through dom-mvc

Welcome to the dom-mvc tutorial. This as an adaption of the
example application from the
[rhone-js](https://github.com/stefan-hoeck/idris2-rhone-js),
library I used to use for writing interactive single page web
applications in Idris, but which turned out to be overly
complicated for most tasks. Here, we take a simpler and -
in my opinion - more satisfying approach.

## Prerequisites

Most posts in this tutorial are literate Idris2 files that
can be built and tried in your own browser. All you have to do
is to install the
[pack](https://github.com/stefan-hoeck/idris2-pack),
package manager, run `make page` from the project's root directory and
load `mvc.html` in your browser afterwards.

## The `main` Function

This is the example project's main module, and here is
the code:

```idris
module Examples.Main

import JS
import Examples.Selector

covering
main : IO ()
main = runJS ui
```

This just imports and runs the user interface `ui` defined in module
`Examples.Selector`. We will have a closer
look at function `runMVC` once we understand the general
structure of a dom-mvc project. Function `runJS` comes from
the [idris2-js](https://github.com/stefan-hoeck/idris2-dom/)
library: To properly deal with the uncertainties of the
JavaScript language, the core IO type we use most of the time
is `JSIO`, which is an alias for `EitherT JSErr IO`, where
`JSErr` is an error type defined also in idris2-js.
Function `runJS` breaks out of the `Either` monad by logging
all errors to the console. This might not be the best solution for a
real-world application, as the console logs in the browser
will not be inspected by the average user, but it will do
for these tutorials.

## What next?

Jump to the [examples selector implementation](Selector.md), to learn about the
general structure of an interactive dom-mvc web page.

<!-- vi: filetype=idris2:syntax=markdown
-->
