# A Guided Tour through rhone-js

Welcome to the rhone-js tutorial. Everything's very new here and
bound to change a lot in the near future. Still, I'll try and evolve
this tutorial alongside the library and provide examples of
increasing complexity, both as documentation and to extend
my own skills in writing web applications in Idris2.

## Prerequisites

All posts in this tutorial are literate Idris2 files that
can be built and tried in your own browser. All you have to do
is install the necessary dependencies listed in the project
README, run `make page` from the project's root directory and
load `rhone.html` in your browser afterwards.

## The `main` Function

This is the example project's main module, and here is
the code:

```idris
module Examples.Main

import Examples.Selector

covering
main : IO ()
main = runJS . ignore $ runMVC (inject "init") init ui
```

This just imports and runs the user interface `ui` defined in module
`Examples.Selector`. We will have a closer
look at function `reactimateIni` once we understand the general
structure of a rhone-js project. Function `runJS` comes from
the [idris2-dom](https://github.com/stefan-hoeck/idris2-dom)
library: To properly deal with the uncertainties of the
JavaScript language, the core IO type we use most of the time
is `JSIO`, which is an alias for `EitherT JSErr IO`, where
`JSErr` is an error type defined also in idris2-dom.
Function `runJS` breaks out of the `Either` monad, by logging
all errors to the console. This might not be the best solution for a
real-world application, as the console logs in the browser
will not be inspected by the average user, but it will do
for these tutorials.

## What next?

Jump to the [examples selector implementation](Selector.md), to learn about the
general structure of an interactive rhone-js web page.

<!-- vi: filetype=idris2:syntax=markdown
-->
