module Web.MVC.Reactimate

import Control.Monad.Either.Extra
import Data.List.Quantifiers.Extra
import JS
import Text.HTML
import Web.Dom
import Web.Html
import Web.MVC.Canvas
import Web.MVC.DOMUpdate
import Web.MVC.Event
import Web.MVC.Output

%default total

--------------------------------------------------------------------------------
--          Event Handler
--------------------------------------------------------------------------------

||| Type alias for an event handler
public export
0 Handler : Type -> Type
Handler e = e -> JSIO ()

||| Type alias for a handler of a list of possible events
public export
0 SHandler : List Type -> Type
SHandler es = HSum es -> JSIO ()

--------------------------------------------------------------------------------
--          Registering Events
--------------------------------------------------------------------------------

||| Low level method for registering `DOMEvents` at
||| HTML elements.
|||
||| Use this, for instance, to register `DOMEvents` at
||| a HTMLElement of a static document.
export
registerDOMEvent : Handler e -> EventTarget -> DOMEvent e -> JSIO ()
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
           (handle : Handler e)

  ||| Manually register an event handler at the given element
  export
  handleEvent : Ref t -> DOMEvent e -> JSIO ()
  handleEvent ref de = do
    el  <- castElementByRef ref
    registerDOMEvent handle el de

  export
  setAttribute : Element -> Attribute t e -> JSIO ()
  setAttribute el (Id (Id value))   = setAttribute el "id" value
  setAttribute el (Str name value)  = setAttribute el name value
  setAttribute el (Bool name value) = case value of
    True  => setAttribute el name ""
    False => removeAttribute el name
  setAttribute el (Event ev) = registerDOMEvent handle (up el) ev

  export
  setAttributeRef : Ref t -> Attribute t e -> JSIO ()
  setAttributeRef ref a = do
    el <- castElementByRef ref
    setAttribute el a

  export
  setAttributesRef : Ref t -> List (Attribute t e) -> JSIO ()
  setAttributesRef el = traverseList_ (setAttributeRef el)

--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

parameters {0    e : Type}      -- event type
           (handle : Handler e) -- event handler

  createNode : Document -> String -> List (Attribute t e) -> JSIO Element
  createNode doc str xs = do
    el <- createElement doc str
    traverseList_ (setAttribute handle el) xs
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
  addNode doc p (El {tag} _ xs ys) = do
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

  addNodes doc p = assert_total $ traverseList_ (addNode doc p)

  setupNodes :
       (Element -> DocumentFragment -> JSIO ())
    -> Ref t
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
    -> Ref t
    -> Node e
    -> JSIO ()
  setupNode adj ref n = setupNodes adj ref [n]

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them as the children of the given target.
  export %inline
  innerHtmlAtN : Ref t -> List (Node e) -> JSIO ()
  innerHtmlAtN = setupNodes replaceChildren

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it as the only child of the given target.
  export %inline
  innerHtmlAt : Ref t -> Node e -> JSIO ()
  innerHtmlAt = setupNode replaceChildren

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them after the given child node.
  export %inline
  afterN : Ref t -> List (Node e) -> JSIO ()
  afterN = setupNodes afterDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it after the given child node.
  export %inline
  after : Ref t -> Node e -> JSIO ()
  after = setupNode afterDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them before the given child node.
  export %inline
  beforeN : Ref t -> List (Node e) -> JSIO ()
  beforeN = setupNodes beforeDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it before the given child node.
  export %inline
  before : Ref t -> Node e -> JSIO ()
  before = setupNode beforeDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| appends them to the given element's list of children
  export %inline
  appendN : Ref t -> List (Node e) -> JSIO ()
  appendN = setupNodes appendDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| appends it to the given element's list of children
  export %inline
  append : Ref t -> Node e -> JSIO ()
  append = setupNode appendDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| prepends them to the given element's list of children
  export %inline
  prependN : Ref t -> List (Node e) -> JSIO ()
  prependN = setupNodes prependDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| prepends it to the given element's list of children
  export %inline
  prepend : Ref t -> Node e -> JSIO ()
  prepend = setupNode prependDF

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| replaces the given element.
  export %inline
  replaceN : Ref t -> List (Node e) -> JSIO ()
  replaceN = setupNodes replaceDF

  ||| Sets up the reactive behavior of the given `Node` and
  ||| replaces the given element.
  export %inline
  replace : Ref t -> Node e -> JSIO ()
  replace = setupNode replaceDF

  ||| Execute a single DOM update instruction
  export
  updateDOM1 : DOMUpdate e -> JSIO ()
  updateDOM1 (Render x s)      = render x s
  updateDOM1 (Children x ns)   = innerHtmlAtN x ns
  updateDOM1 (Replace x ns)    = replaceN x ns
  updateDOM1 (Append x ns)     = appendN x ns
  updateDOM1 (Prepend x ns)    = prependN x ns
  updateDOM1 (After x ns)      = afterN x ns
  updateDOM1 (Before x ns)     = beforeN x ns
  updateDOM1 (Attr x a)        = setAttributeRef handle x a
  updateDOM1 (Value x a)       = setValue x a
  updateDOM1 (ValidityMsg x a) = setValidityMessage x a
  updateDOM1 (Remove x)        = castElementByRef {t = Element} x >>= remove
  updateDOM1 NoAction          = pure ()

  ||| Execute several DOM update instructions
  export %inline
  updateDOM : List (DOMUpdate e) -> JSIO ()
  updateDOM = traverseList_ updateDOM1

export
runDOM :
     (adjST   : e -> s -> s)
  -> (display : e -> s -> List (DOMUpdate e))
  -> (handler : Handler e)
  -> (event   : e)
  -> (state   : s)
  -> JSIO s
runDOM adjST display handler event state =
  let st2 := adjST event state
   in updateDOM handler (display event st2) $> st2

export %inline
injectDOM :
     {auto has : Has e es}
  -> (adjST   : e -> s -> s)
  -> (display : e -> s -> List (DOMUpdate e))
  -> (handler : SHandler es)
  -> (event   : e)
  -> (state   : s)
  -> JSIO s
injectDOM f g h = runDOM f g (h . inject)
