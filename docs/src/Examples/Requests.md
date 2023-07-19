# Sending HTTP Requests

This short tutorial explains how to send and process HTTP
requests. It was inspired by a
[similar tutorial](https://guide.elm-lang.org/effects/json.html)
for the Elm programming language, and shamelessly accesses their
server to fetch a random quote.

The stuff required for running HTTP requests can be found in
module `Web.MVC.Http`:

```idris
module Examples.Requests

import Derive.Prelude
import JSON.Simple.Derive
import Examples.CSS.Requests
import Examples.Util
import Web.MVC
import Web.MVC.Http

%default total
%language ElabReflection
```

## Model

When sending a request to the server at "https://elm-lang.org/api/random-quotes",
the response is a quote plus some information encoded in a JSON object.
We therefore first need to decode the JSON object in question and
store its content in a record type:

```idris
public export
record Quote where
  constructor Q
  quote  : String
  source : String
  author : String
  year   : Bits16

%runElab derive "Quote" [Show,Eq,FromJSON,ToJSON]
```

Our even type is very basic: An event to initialize the page,
one to send a new request, and one holding the server's response:

```idris
public export
data ReqEv : Type where
  ReqInit  : ReqEv
  GetQuote : ReqEv
  Got      : Either HTTPError Quote -> ReqEv
```

The state type is even trivial: There is no state of interest, and I'm just
using a placeholder to align it with the other example apps.

```idris
public export
data ReqST = RS
```

## View

The view just holds a button and some space for printing
the quote plus some additional info.

```idris
content : Node ReqEv
content =
  div [ class requestContent ]
    [ button [onClick GetQuote, classes [quoteBtn, widget, btn]] ["get quote"]
    , blockquote [ Id quote ] [ ]
    , p [ Id quoteInfo ] []
    ]
```

The actual result is only displayed if we get it from the server:

```idris
printError : HTTPError -> String
printError Timeout = "connection timed out"
printError NetworkError = "error when connecting to server"
printError (BadStatus m) = "server responded with bad status code: \{show m}"
printError (JSONError str x) =
  """
  Error when decoding JSON string: \{str}

  \{prettyErr str x}
  """

dispResult : Either HTTPError Quote -> List (Node ReqEv)
dispResult (Left x)  = [ div [class requestError ] [ Text $ printError x] ]
dispResult (Right q) =
  [ Text "— "
  , div [] [ Text q.source ]
  , Text " by \{q.author} (\{show q.year})"
  ]
```

## Controller

Finally, the controller: The only new part is where we send a
HTTP request when event `GetQuote` occurs:

```idris
display : ReqEv -> ReqST -> Cmds ReqEv
display ReqInit  s = [child exampleDiv content]
display GetQuote s = [getJSON "https://elm-lang.org/api/random-quotes" Got]
display (Got x)  s =
  [ children quoteInfo (dispResult x)
  , text quote $ either (const "") quote x
  ]

export
runReq : Handler ReqEv => Controller ReqST ReqEv
runReq = runDOM (const id) display
```

<!-- vi: filetype=idris2:syntax=markdown
-->

