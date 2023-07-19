module Web.MVC.Reactimate

import Control.Monad.Either.Extra
import Data.List.Quantifiers.Extra
import Data.Either
import Data.Maybe
import Data.String
import JS
import Text.CSS
import Text.HTML
import Web.Dom
import Web.Html
import Web.MVC.Canvas
import Web.MVC.Event
import Web.MVC.Output

%default total

--------------------------------------------------------------------------------
--          Event Handler
--------------------------------------------------------------------------------

||| Type alias for an event handler
public export
record Handler (e : Type) where
  [noHints]
  constructor H
  handle_ : e -> JSIO ()

export %inline
handle : Handler e => e -> JSIO ()
handle = handle_ %search

export %inline
inject : Has e es => Handler (HSum es) -> Handler e
inject (H h) = H (h . inject)

--------------------------------------------------------------------------------
--          Registering Events
--------------------------------------------------------------------------------

parameters {0 e    : Type}
           {auto h : Handler e}

  ||| Low level method for registering `DOMEvents` at
  ||| HTML elements.
  |||
  ||| Use this, for instance, to register `DOMEvents` at
  ||| a HTMLElement of a static document.
  export
  registerDOMEvent : EventTarget -> DOMEvent e -> JSIO ()
  registerDOMEvent el de = case de of
    Input f      => inst "input" inputInfo f
    Change f     => inst "change" changeInfo f
    Click f      => inst "click" mouseInfo f
    DblClick f   => inst "dblclick" mouseInfo f
    KeyDown f    => inst "keydown" keyInfo f
    KeyUp f      => inst "keyup" keyInfo f
    Blur v       => inst "blur" {t = Event} (const $ pure v) Just
    Focus v      => inst "focus" {t = Event} (const $ pure v) Just
    MouseDown f  => inst "mousedown" mouseInfo f
    MouseUp f    => inst "mouseup" mouseInfo f
    MouseEnter f => inst "mouseenter" mouseInfo f
    MouseLeave f => inst "mouseleave" mouseInfo f
    MouseOver f  => inst "mouseover" mouseInfo f
    MouseOut f   => inst "mouseout" mouseInfo f
    MouseMove f  => inst "mousemove" mouseInfo f
    HashChange v => inst "hashchange" {t = Event} (const $ pure v) Just

    where
      inst :
           {0 t,b : _}
        -> {auto c : SafeCast t}
        -> String
        -> (t -> JSIO b)
        -> (b -> Maybe e)
        -> JSIO ()
      inst {t} s conv f = do
        c <- callback {cb = EventListener} $ \e => do
          va <- tryCast_ t "Control.Monad.Dom.Interface.inst" e
          conv va >>= maybe (pure ()) handle . f

        addEventListener el s (Just c)

  ||| Manually register an event handler at the given element
  export
  handleEvent : Ref t -> DOMEvent e -> JSIO ()
  handleEvent r de = do
    el  <- castElementByRef r
    registerDOMEvent el de

  export
  setAttribute : Element -> Attribute t e -> JSIO ()
  setAttribute el (Id (Id value))   = setAttribute el "id" value
  setAttribute el (Str name value)  = setAttribute el name value
  setAttribute el (Bool name value) = case value of
    True  => setAttribute el name ""
    False => removeAttribute el name
  setAttribute el (Event ev) = registerDOMEvent (up el) ev
  setAttribute el Empty      = pure ()


--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

  createNode : Document -> String -> List (Attribute t e) -> JSIO Element
  createNode doc str xs = do
    el <- createElement doc str
    traverseList_ (setAttribute el) xs
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

public export
record DOMUpdate (e : Type) where
  constructor D
  run : Handler e => JSIO ()

export %inline
noAction : DOMUpdate e
noAction = D (pure ())

setupNodes :
     (Element -> DocumentFragment -> JSIO ())
  -> Ref t
  -> List (Node e)
  -> DOMUpdate e
setupNodes adj r ns = D $ do
  doc  <- document
  elem <- castElementByRef {t = Element} r
  df   <- createDocumentFragment doc
  addNodes doc df ns
  adj elem df

%inline
setupNode :
     (Element -> DocumentFragment -> JSIO ())
  -> Ref t
  -> Node e
  -> DOMUpdate e
setupNode adj r n = setupNodes adj r [n]

||| Execute several DOM update instructions
export %inline
updateDOM : Handler e => List (DOMUpdate e) -> JSIO ()
updateDOM = traverseList_ (\x => x.run)

||| Sets up the reactive behavior of the given `Node`s and
||| inserts them as the children of the given target.
export %inline
children : Ref t -> List (Node e) -> DOMUpdate e
children = setupNodes replaceChildren

||| Sets up the reactive behavior of the given `Node` and
||| inserts it as the only child of the given target.
export %inline
child : Ref t -> Node e -> DOMUpdate e
child = setupNode replaceChildren

||| Replaces the given node's children with a text node
||| displaying the given string.
export %inline
text : Ref t -> String -> DOMUpdate e
text r = child r . Text

||| Replaces the given node's children with a text node
||| showing the given value.
export %inline
show : Show b => Ref t -> b -> DOMUpdate e
show r = text r . show

||| Replaces the given node's children with the raw
||| HTML passed as a string argument.
export %inline
raw : Ref t -> String -> DOMUpdate e
raw r = child r . Raw

||| Replaces the given `<style>` node's CSS rules.
export
style : Ref Tag.Style -> List (Rule 1) -> DOMUpdate e
style r rules =
  let str := fastUnlines $ map interpolate rules
   in raw r str

||| Sets up the reactive behavior of the given `Node`s and
||| inserts them after the given child node.
export %inline
afterMany : Ref t -> List (Node e) -> DOMUpdate e
afterMany = setupNodes afterDF

||| Sets up the reactive behavior of the given `Node` and
||| inserts it after the given child node.
export %inline
after : Ref t -> Node e -> DOMUpdate e
after = setupNode afterDF

||| Sets up the reactive behavior of the given `Node`s and
||| inserts them before the given child node.
export %inline
beforeMany : Ref t -> List (Node e) -> DOMUpdate e
beforeMany = setupNodes beforeDF

||| Sets up the reactive behavior of the given `Node` and
||| inserts it before the given child node.
export %inline
before : Ref t -> Node e -> DOMUpdate e
before = setupNode beforeDF

||| Sets up the reactive behavior of the given `Node`s and
||| appends them to the given element's list of children
export %inline
appendMany : Ref t -> List (Node e) -> DOMUpdate e
appendMany = setupNodes appendDF

||| Sets up the reactive behavior of the given `Node` and
||| appends it to the given element's list of children
export %inline
append : Ref t -> Node e -> DOMUpdate e
append = setupNode appendDF

||| Sets up the reactive behavior of the given `Node`s and
||| prepends them to the given element's list of children
export %inline
prependMany : Ref t -> List (Node e) -> DOMUpdate e
prependMany = setupNodes prependDF

||| Sets up the reactive behavior of the given `Node` and
||| prepends it to the given element's list of children
export %inline
prepend : Ref t -> Node e -> DOMUpdate e
prepend = setupNode prependDF

||| Sets up the reactive behavior of the given `Node`s and
||| replaces the given element.
export %inline
replaceMany : Ref t -> List (Node e) -> DOMUpdate e
replaceMany = setupNodes replaceDF

||| Sets up the reactive behavior of the given `Node` and
||| replaces the given element.
export %inline
replace : Ref t -> Node e -> DOMUpdate e
replace = setupNode replaceDF

||| Sets a custom validity message at the given node.
export %inline
validityMsg : Ref t -> ValidityTag t => String -> DOMUpdate e
validityMsg r s = D $ setValidityMessage r s

||| Sets or unsets a custom validity message at the given node.
export
validate : Ref t -> ValidityTag t => Either String b -> DOMUpdate e
validate r (Left s)  = validityMsg r s
validate r (Right s) = validityMsg r ""

||| Sets an attribute at the given node.
export
attr : Ref t -> Attribute t e -> DOMUpdate e
attr r a = D $ castElementByRef r >>= \el => setAttribute el a

||| Sets the `disabled` attribute of the given element
export %inline
disabled : Ref t -> Bool -> DOMUpdate e
disabled r = attr r . disabled

||| Sets the `disabled` attribute of the given element
||| if the given values is a `Left`.
|||
||| This is useful for disabling components such as buttons
||| in the UI in case of invalid user input.
export %inline
disabledE : {0 a,b : _} -> Ref t -> Either a b -> DOMUpdate e
disabledE r = disabled r . isLeft

||| Sets the `disabled` attribute of the given element
||| if the given values is a `Nothing`.
|||
||| This is useful for disabling components such as buttons
||| in the UI in case of invalid user input.
export %inline
disabledM : {0 a : _} -> Ref t -> Maybe a -> DOMUpdate e
disabledM r = disabled r . isNothing

||| Removes the given element from the DOM.
export
remove : Ref t -> DOMUpdate e
remove r = D (castElementByRef {t = Element} r >>= remove)

||| Sets the `value` attribute of the given element.
export %inline
value : Ref t -> ValueTag t => String -> DOMUpdate e
value r s = D (setValue r s)

||| Renders a scene at a canvas element
export %inline
render : Ref Tag.Canvas -> Scene -> DOMUpdate e
render r s = D (render r s)

||| Focus the given HTMLElemet
export %inline
focus : Ref t -> DOMUpdate e
focus r = D (castElementByRef {t = HTMLElement} r >>= HTMLOrSVGElement.focus)

export
updateIf : Bool -> Lazy (DOMUpdate e) -> DOMUpdate e
updateIf True  u = u
updateIf False _ = noAction

export
runDOM :
     {auto h : Handler e}
  -> (adjST   : e -> s -> s)
  -> (display : e -> s -> List (DOMUpdate e))
  -> (event   : e)
  -> (state   : s)
  -> JSIO s
runDOM adjST display event state =
  let st2 := adjST event state
   in updateDOM (display event st2) $> st2
