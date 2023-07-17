module Web.MVC.Reactimate

import Data.IORef
import Data.Nat
import JS
import Web.MVC.ElemRef
import Web.MVC.Event
import Text.CSS
import Text.HTML
import Web.Dom
import Web.Html

%default total

primTraverse_ : (t -> JSIO ()) -> List t -> PrimIO (Either JSErr ())
primTraverse_ f []        w = MkIORes (Right ()) w
primTraverse_ f (x :: xs) w =
  let MkIORes (Right ()) w2 := toPrim (runEitherT (f x)) w
        | MkIORes (Left err) w2 => MkIORes (Left err) w2
   in primTraverse_ f xs w2

-- TODO : This should got to the JS module
export
traverseJSIO_ : (t -> JSIO ()) -> List t -> JSIO ()
traverseJSIO_ f xs = MkEitherT $ fromPrim $ primTraverse_ f xs

-- TODO : This should got to the JS module
export %inline
forJSIO_ : List t -> (t -> JSIO ()) -> JSIO ()
forJSIO_ as f = traverseJSIO_ f as

||| Low level method for registering `DOMEvents` at
||| HTML elements.
|||
||| Use this, for instance, to register `DOMEvents` at
||| a HTMLElement of a static document.
export
registerDOMEvent : (e -> JSIO ()) -> EventTarget -> DOMEvent e -> JSIO ()
registerDOMEvent handle el de = case de of
  Input f      => inst "input" inputInfo f
  Change f     => inst "change" changeInfo f
  Click f      => inst "click" mouseInfo f
  DblClick f   => inst "dblclick" mouseInfo f
  KeyDown f    => inst "keydown" keyInfo f
  KeyUp f      => inst "keyup" keyInfo f
  Blur v       => inst "blur" {a = Event} (const $ pure v) Just
  Focus v      => inst "focus" {a = Event} (const $ pure v) Just
  MouseDown f  => inst "mousedown" mouseInfo f
  MouseUp f    => inst "mouseup" mouseInfo f
  MouseEnter f => inst "mouseenter" mouseInfo f
  MouseLeave f => inst "mouseleave" mouseInfo f
  MouseOver f  => inst "mouseover" mouseInfo f
  MouseOut f   => inst "mouseout" mouseInfo f
  MouseMove f  => inst "mousemove" mouseInfo f
  HashChange v => inst "hashchange" {a = Event} (const $ pure v) Just

  where
    inst :
         {0 a,b : _}
      -> {auto c : SafeCast a}
      -> String
      -> (a -> JSIO b)
      -> (b -> Maybe e)
      -> JSIO ()
    inst s conv f = do
      c <- callback {cb = EventListener} $ \e => do
        va <- tryCast_ a "Control.Monad.Dom.Interface.inst" e
        conv va >>= maybe (pure ()) handle . f

      addEventListener el s (Just c)

parameters {0    e : Type}
           (handle : e -> JSIO ())

  ||| Manually register an event handler at the given element
  export
  handleEvent : ElemRef -> DOMEvent e -> JSIO ()
  handleEvent ref de = do
    el  <- castElementByRef ref
    registerDOMEvent handle el de

  export
  setAttribute : Element -> Attribute e -> JSIO ()
  setAttribute el (Id value)        = setAttribute el "id" value
  setAttribute el (Str name value)  = setAttribute el name value
  setAttribute el (Bool name value) = case value of
    True  => setAttribute el name ""
    False => removeAttribute el name
  setAttribute el (Event ev) = registerDOMEvent handle (up el) ev

  export
  setAttributeRef : ElemRef -> Attribute e -> JSIO ()
  setAttributeRef ref a = do
    el <- castElementByRef ref
    setAttribute el a

  export
  setAttributesRef : ElemRef -> List (Attribute e) -> JSIO ()
  setAttributesRef el = traverseJSIO_ (setAttributeRef el)

--------------------------------------------------------------------------------
--          DOM Update
--------------------------------------------------------------------------------

nodeList : DocumentFragment -> List (HSum [Node,String])
nodeList df = [inject $ df :> Node]

||| Replaces all children of the given node with a new document fragment.
export %inline
replaceChildren : Element -> DocumentFragment -> JSIO ()
replaceChildren elem = replaceChildren elem . nodeList

||| Appends the given document fragment to a DOM element's children
export %inline
appendDF : Element -> DocumentFragment -> JSIO ()
appendDF elem = append elem . nodeList

||| Prepends the given document fragment to a DOM element's children
export %inline
prependDF : Element -> DocumentFragment -> JSIO ()
prependDF elem = prepend elem . nodeList

||| Inserts the given document fragment after a DOM element.
export %inline
afterDF : Element -> DocumentFragment -> JSIO ()
afterDF elem = after elem . nodeList

||| Inserts the given document fragment before a DOM element.
export %inline
beforeDF : Element -> DocumentFragment -> JSIO ()
beforeDF elem = before elem . nodeList

||| Inserts the given document fragment before a DOM element.
export %inline
replaceDF : Element -> DocumentFragment -> JSIO ()
replaceDF elem = replaceWith elem . nodeList

public export
data DOMUpdate : Type -> Type where
  Children : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Replace  : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Append   : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Prepend  : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  After    : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Before   : ElemRef -> (ns : List (Node e)) -> DOMUpdate e
  Attr     : ElemRef -> Attribute e -> DOMUpdate e
  Remove   : ElemRef -> DOMUpdate e

--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

parameters {0    e : Type}         -- event type
           (handle : e -> JSIO ()) -- event handler

  createNode : Document -> String -> List (Attribute e) -> JSIO Element
  createNode doc str xs = do
    el <- createElement doc str
    traverseJSIO_ (setAttribute handle el) xs
    pure el

  addNodes :
       {auto 0 _ : JSType t}
    -> {auto 0 _ : Elem ParentNode (Types t)}
    -> (doc      : Document)
    -> (parent   : t)
    -> (nodes    : List (Node e))
    -> JSIO ()

  addNode :
       {auto 0 _ : JSType t}
    -> {auto 0 _ : Elem ParentNode (Types t)}
    -> (doc      : Document)
    -> (parent   : t)
    -> (node     : Node e)
    -> JSIO ()
  addNode doc p (El tag xs ys) = do
    n <- createNode doc tag xs
    append p [inject $ n :> Node]
    addNodes doc n ys
  addNode doc p (Raw str) = do
    el <- createElement doc "template"
    Just temp <- pure (castTo HTMLTemplateElement el) | Nothing => pure ()
    innerHTML temp .= str
    c         <- content temp
    append p [inject $ c :> Node]

  addNode doc p (Text str) = append p [inject str]

  addNode doc p Empty      = pure ()

  addNodes doc p = assert_total $ traverseJSIO_ (addNode doc p)

  setupNodes :
       (Element -> DocumentFragment -> JSIO ())
    -> ElemRef
    -> List (Node e)
    -> JSIO ()
  setupNodes adj ref ns = do
    doc  <- document
    elem <- castElementByRef {t = Element} ref
    df   <- createDocumentFragment doc
    addNodes doc df ns
    adj elem df

  %inline
  setupNode :
       (Element -> DocumentFragment -> JSIO ())
    -> ElemRef
    -> Node e
    -> JSIO ()
  setupNode adj ref n = setupNodes adj ref [n]

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them as the children of the given target.
  export %inline
  innerHtmlAtN : ElemRef -> List (Node e) -> JSIO ()
  innerHtmlAtN = setupNodes replaceChildren

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it as the only child of the given target.
  export %inline
  innerHtmlAt : ElemRef -> Node e -> JSIO ()
  innerHtmlAt = setupNode replaceChildren

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them after the given child node.
  export %inline
  afterN : ElemRef -> List (Node e) -> JSIO ()
  afterN = setupNodes afterDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it after the given child node.
  export %inline
  after : ElemRef -> Node e -> JSIO ()
  after = setupNode afterDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them before the given child node.
  export %inline
  beforeN : ElemRef -> List (Node e) -> JSIO ()
  beforeN = setupNodes beforeDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it before the given child node.
  export %inline
  before : ElemRef -> Node e -> JSIO ()
  before = setupNode beforeDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| appends them to the given element's list of children
  export %inline
  appendN : ElemRef -> List (Node e) -> JSIO ()
  appendN = setupNodes appendDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| appends it to the given element's list of children
  export %inline
  append : ElemRef -> Node e -> JSIO ()
  append = setupNode appendDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| prepends them to the given element's list of children
  export %inline
  prependN : ElemRef -> List (Node e) -> JSIO ()
  prependN = setupNodes prependDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| prepends it to the given element's list of children
  export %inline
  prepend : ElemRef -> Node e -> JSIO ()
  prepend = setupNode prependDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| replaces the given element.
  export %inline
  replaceN : ElemRef -> List (Node e) -> JSIO ()
  replaceN = setupNodes replaceDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| replaces the given element.
  export %inline
  replace : ElemRef -> Node e -> JSIO ()
  replace = setupNode replaceDF

  ||| Execute a single DOM update instruction
  export
  updateDOM1 : DOMUpdate e -> JSIO ()
  updateDOM1 (Children x ns) = innerHtmlAtN x ns
  updateDOM1 (Replace x ns)  = replaceN x ns
  updateDOM1 (Append x ns)   = appendN x ns
  updateDOM1 (Prepend x ns)  = prependN x ns
  updateDOM1 (After x ns)    = afterN x ns
  updateDOM1 (Before x ns)   = beforeN x ns
  updateDOM1 (Attr x a)      = setAttributeRef handle x a
  updateDOM1 (Remove x)      = castElementByRef {t = Element} x >>= remove

  ||| Execute several DOM update instructions
  export %inline
  updateDOM : List (DOMUpdate e) -> JSIO ()
  updateDOM = traverseJSIO_ updateDOM1
