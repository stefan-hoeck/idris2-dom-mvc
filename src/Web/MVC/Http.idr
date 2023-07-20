module Web.MVC.Http

import Control.Monad.Either.Extra
import Derive.Prelude
import JS
import JSON.Simple
import Web.Html
import Web.Raw.Xhr
import Web.MVC.Cmd

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

export %inline
Cast String JS.ByteString.ByteString where
  cast = believe_me

export %inline
Cast JS.ByteString.ByteString String where
  cast = believe_me

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

||| HTTP methods currently supported.
public export
data Method = GET | POST

%runElab derive "Method" [Show,Eq,Ord]

||| A HTTP header is just a pair of strings.
public export
0 Header : Type
Header = (String,String)

||| Part in a formdata request.
public export
data Part : Type where
  StringPart : (name, value : String) -> Part
  FilePart   : (name : String) -> (file : File) -> Part

||| Body of a HTTP request.
public export
data RequestBody : Type where
  Empty      : RequestBody
  StringBody : (mimeType : String) -> (content : String) -> RequestBody
  JSONBody   : ToJSON a => a -> RequestBody
  FormBody   : List Part -> RequestBody

||| HTTP Errors
public export
data HTTPError : Type where
  Timeout      : HTTPError
  NetworkError : HTTPError
  BadStatus    : Bits16 -> HTTPError
  JSONError    : String -> DecodingErr -> HTTPError

||| Type of expected respons.
|||
||| Every constructor takes a function for wrapping a request
||| result of type `Either HTTPError x` into the result type.
public export
data Expect : Type -> Type where
  ExpectJSON   : FromJSON a => (Either HTTPError a -> r) -> Expect r
  ExpectString : (Either HTTPError String -> r) -> Expect r
  ExpectAny    : (Either HTTPError () -> r) -> Expect r

bodyHeaders : RequestBody -> List Header
bodyHeaders Empty            = []
bodyHeaders (StringBody m _) = [("Content-Type", m)]
bodyHeaders (JSONBody x)     = [("Content-Type", "application/json")]
bodyHeaders (FormBody x)     = [("Content-Type", "multipart/form-data")]

append : FormData -> Part -> JSIO ()
append fd (StringPart name value) = FormData.append  fd name value
append fd (FilePart name file)    = FormData.append1 fd name file

parameters {0 r    : Type}

  onerror : Handler r => Expect r -> HTTPError -> JSIO ()
  onerror (ExpectJSON f)   err = handle (f $ Left err)
  onerror (ExpectString f) err = handle (f $ Left err)
  onerror (ExpectAny f)    err = handle (f $ Left err)

  onsuccess : Handler r => Expect r -> XMLHttpRequest -> JSIO ()
  onsuccess (ExpectString f)   x = responseText x >>= handle . f . Right
  onsuccess (ExpectAny f)      x = handle (f $ Right ())
  onsuccess (ExpectJSON {a} f) x = do
    s <- responseText x
    handle . f . mapFst (JSONError s) $ decode s

  onload : Handler r => Expect r -> XMLHttpRequest -> JSIO ()
  onload exp x = do
    st   <- status x
    case st >= 200 && st < 300 of
      False => onerror exp (BadStatus st)
      True  => onsuccess exp x

  xsend : RequestBody -> XMLHttpRequest -> JSIO ()
  xsend Empty            x = XMLHttpRequest.send x
  xsend (StringBody _ s) x = XMLHttpRequest.send' x (Def . Just $ inject s)
  xsend (JSONBody d)     x = XMLHttpRequest.send' x (Def . Just $ inject $ encode d)
  xsend (FormBody ps)    x = do
    fd <- FormData.new
    traverseList_ (append fd) ps
    XMLHttpRequest.send' x (Def . Just $ inject fd)

  ||| Send a HTTP request.
  export
  request :
       (method  : Method)
    -> (headers : List Header)
    -> (url     : String)
    -> (body    : RequestBody)
    -> (expect  : Expect r)
    -> (timeout : Maybe Bits32)
    -> Cmd r
  request m headers url body exp tout = C $ Prelude.do
    -- create new Http request
    x <- XMLHttpRequest.new

    -- register event listeners
    XMLHttpRequestEventTarget.onerror x ?> onerror exp NetworkError
    XMLHttpRequestEventTarget.onload x ?> onload exp x
    XMLHttpRequestEventTarget.ontimeout x ?> onerror exp Timeout

    -- open url
    open_ x (cast $ show m) url

    -- set message headers
    let hs := bodyHeaders body ++ headers
    traverseList_ (\(n,h) => setRequestHeader x (cast n) (cast h)) hs

    -- set timeout (if any)
    traverse_ (set (timeout x)) tout

    -- send request
    xsend body x

||| Send a GET HTTP request.
export %inline
get : (url : String) -> (expect : Expect r) -> Cmd r
get u e = request GET [] u Empty e Nothing

||| Send a GET request, reading the response as plain text.
export %inline
getText : (url : String) -> (f : Either HTTPError String -> r) -> Cmd r
getText u = get u . ExpectString

||| Send a GET request, decoding the result as a JSON string
||| and converting it to the result type `a`.
export %inline
getJSON : FromJSON a => (url : String) -> (f : Either HTTPError a -> r) -> Cmd r
getJSON u = get u . ExpectJSON

||| Send a POST request.
export %inline
post : (url : String) -> (body : RequestBody) -> (expect : Expect r) -> Cmd r
post u b e = request POST [] u b e Nothing
