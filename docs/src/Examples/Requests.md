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
store its content in a record type. Interface `FromJSON` from the *json-simple*
library is used for decoding JSON objects. It can be derived automatically
via elaborator reflection for regular data types (if you are more interested
in this topic, the [elab-util](https://github.com/stefan-hoeck/idris2-elab-util)
library comes with its own lengthy tutorial).

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

Our event type is very basic: An event to initialize the page,
one to start a new request, and one to hold the server's response:

```idris
public export
data ReqEv : Type where
  ReqInit  : ReqEv
  GetQuote : ReqEv
  Got      : Either HTTPError Quote -> ReqEv
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
export
display : ReqEv -> Cmd ReqEv
display ReqInit  = child exampleDiv content
display GetQuote = getJSON "https://elm-lang.org/api/random-quotes" Got
display (Got x)  =
  batch
    [ children quoteInfo (dispResult x)
    , text quote $ either (const "") quote x
    ]
```

<!-- vi: filetype=idris2:syntax=markdown
-->

