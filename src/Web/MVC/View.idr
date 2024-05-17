module Web.MVC.View

import Control.Monad.Either.Extra
import Data.Either
import Data.Maybe
import Data.String
import JS
import Text.CSS
import Text.HTML
import Web.Dom
import Web.Html
import Web.MVC.Canvas
import Web.MVC.Cmd
import Web.MVC.Event
import Web.MVC.Util

%default total

--------------------------------------------------------------------------------
--          Registering Events
--------------------------------------------------------------------------------

||| Low level method for registering `DOMEvents` at
||| HTML elements.
|||
||| Use this, for instance, to register `DOMEvents` at
||| a HTMLElement of a static document.
export
registerDOMEvent :
     (e -> JSIO ())
  -> (preventDefault, stopPropagation : Bool)
  -> EventTarget
  -> DOMEvent e
  -> JSIO ()
registerDOMEvent h prev stop el de = case de of
  Input f      => inst "input" changeInfo f
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
  Scroll f     => inst "scroll" scrollInfo f
  Wheel f      => inst "wheel" wheelInfo f

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
        canc <- cancelable e
        bubl <- bubbles e
        when (canc && prev) (preventDefault e)
        when (bubl && stop) (stopPropagation e)
        va <- tryCast_ t "Web.MVC.View.inst" e
        conv va >>= maybe (pure ()) h . f

      addEventListener el s (Just c)

export
setAttribute : (e -> JSIO ()) -> Element -> Attribute t e -> JSIO ()
setAttribute h el (Id (Id value))   = setAttribute el "id" value
setAttribute h el (Str name value)  = setAttribute el name value
setAttribute h el (Bool name value) = case value of
  True  => setAttribute el name ""
  False => removeAttribute el name
setAttribute h el (Event_ prev stop ev) = registerDOMEvent h prev stop(up el) ev
setAttribute h el Empty      = pure ()


--------------------------------------------------------------------------------
--          Cursor Types
--------------------------------------------------------------------------------

||| All standard mouse cursors.
|||
||| For using custom icons, use type `Url` linking to an
||| image and choose the offset for the mouse position.
export
data Cursor : Type where
  Alias       : Cursor
  AllScroll   : Cursor
  Auto        : Cursor
  Cell        : Cursor
  ColResize   : Cursor
  ContextMenu : Cursor
  Copy        : Cursor
  Crosshair   : Cursor
  Default     : Cursor
  EResize     : Cursor
  EwResize    : Cursor
  Grab        : Cursor
  Grabbing    : Cursor
  Help        : Cursor
  Move        : Cursor
  NResize     : Cursor
  NeResize    : Cursor
  NeswResize  : Cursor
  NsResize    : Cursor
  NwResize    : Cursor
  NwseResize  : Cursor
  NoDrop      : Cursor
  None        : Cursor
  NotAllowed  : Cursor
  Pointer     : Cursor
  Progress    : Cursor
  RowResize   : Cursor
  SResize     : Cursor
  SeResize    : Cursor
  SwResize    : Cursor
  Text        : Cursor
  Url         : (url : String) -> (xOffset,yOffset : Nat) -> Cursor
  WResize     : Cursor
  Wait        : Cursor
  ZoomIn      : Cursor
  ZoomOut     : Cursor

export
Interpolation Cursor where
  interpolate Alias         = "alias"
  interpolate AllScroll     = "all-scroll"
  interpolate Auto          = "auto"
  interpolate Cell          = "cell"
  interpolate ColResize     = "col-resize"
  interpolate ContextMenu   = "context-menu"
  interpolate Copy          = "copy"
  interpolate Crosshair     = "crosshair"
  interpolate Default       = "default"
  interpolate EResize       = "e-resize"
  interpolate EwResize      = "ew-resize"
  interpolate Grab          = "grab"
  interpolate Grabbing      = "grabbing"
  interpolate Help          = "help"
  interpolate Move          = "move"
  interpolate NResize       = "n-resize"
  interpolate NeResize      = "ne-resize"
  interpolate NeswResize    = "nesw-resize"
  interpolate NsResize      = "ns-resize"
  interpolate NwResize      = "nw-resize"
  interpolate NwseResize    = "nwse-resize"
  interpolate NoDrop        = "no-drop"
  interpolate None          = "none"
  interpolate NotAllowed    = "not-allowed"
  interpolate Pointer       = "pointer"
  interpolate Progress      = "progress"
  interpolate RowResize     = "row-resize"
  interpolate SResize       = "s-resize"
  interpolate SeResize      = "se-resize"
  interpolate SwResize      = "sw-resize"
  interpolate Text          = "text"
  interpolate (Url url x y) = "url(\{show url}) \{show x} \{show y}, auto"
  interpolate WResize       = "w-resize"
  interpolate Wait          = "wait"
  interpolate ZoomIn        = "zoom-in"
  interpolate ZoomOut       = "zoom-out"

--------------------------------------------------------------------------------
--          Node Preparation
--------------------------------------------------------------------------------

addNodes :
     {auto 0 _ : JSType t}
  -> {auto 0 _ : Elem ParentNode (Types t)}
  -> (h        : e -> JSIO ())
  -> (doc      : Document)
  -> (parent   : t)
  -> (nodes    : List (Node e))
  -> JSIO ()

addNode :
     {auto 0 _ : JSType t}
  -> {auto 0 _ : Elem ParentNode (Types t)}
  -> (h        : e -> JSIO ())
  -> (doc      : Document)
  -> (parent   : t)
  -> (node     : Node e)
  -> JSIO ()
addNode h doc p (El {tag} _ xs ys) = do
  n <- createElement doc tag
  append p [inject $ n :> Node]
  addNodes h doc n ys
  traverseList_ (setAttribute h n) xs

addNode h doc p (Raw str) = do
  el <- createElement doc "template"
  Just temp <- pure (castTo HTMLTemplateElement el) | Nothing => pure ()
  innerHTML temp .= str
  c         <- content temp
  append p [inject $ c :> Node]

addNode h doc p (Text str) = append p [inject str]

addNode h doc p Empty      = pure ()

addNodes h doc p = assert_total $ traverseList_ (addNode h doc p)

setupNodes :
     (Element -> DocumentFragment -> JSIO ())
  -> Ref t
  -> List (Node e)
  -> Cmd e
setupNodes adj r ns = C $ \h => do
  doc  <- document
  elem <- castElementByRef {t = Element} r
  df   <- createDocumentFragment doc
  addNodes h doc df ns
  adj elem df

%inline
setupNode :
     (Element -> DocumentFragment -> JSIO ())
  -> Ref t
  -> Node e
  -> Cmd e
setupNode adj r n = setupNodes adj r [n]

||| Sets up the reactive behavior of the given `Node`s and
||| inserts them as the children of the given target.
export %inline
children : Ref t -> List (Node e) -> Cmd e
children = setupNodes replaceChildren

||| Sets up the reactive behavior of the given `Node` and
||| inserts it as the only child of the given target.
export %inline
child : Ref t -> Node e -> Cmd e
child = setupNode replaceChildren

||| Replaces the given node's children with a text node
||| displaying the given string.
export %inline
text : Ref t -> String -> Cmd e
text r = child r . Text

||| Replaces the given node's children with a text node
||| showing the given value.
export %inline
show : Show b => Ref t -> b -> Cmd e
show r = text r . show

||| Replaces the given node's children with the raw
||| HTML passed as a string argument.
export %inline
raw : Ref t -> String -> Cmd e
raw r = child r . Raw

||| Replaces the given `<style>` node's CSS rules.
export
style : Ref Tag.Style -> List (Rule 1) -> Cmd e
style r rules =
  let str := fastUnlines $ map interpolate rules
   in raw r str

||| Sets up the reactive behavior of the given `Node`s and
||| inserts them after the given child node.
export %inline
afterMany : Ref t -> List (Node e) -> Cmd e
afterMany = setupNodes afterDF

||| Sets up the reactive behavior of the given `Node` and
||| inserts it after the given child node.
export %inline
after : Ref t -> Node e -> Cmd e
after = setupNode afterDF

||| Sets up the reactive behavior of the given `Node`s and
||| inserts them before the given child node.
export %inline
beforeMany : Ref t -> List (Node e) -> Cmd e
beforeMany = setupNodes beforeDF

||| Sets up the reactive behavior of the given `Node` and
||| inserts it before the given child node.
export %inline
before : Ref t -> Node e -> Cmd e
before = setupNode beforeDF

||| Sets up the reactive behavior of the given `Node`s and
||| appends them to the given element's list of children
export %inline
appendMany : Ref t -> List (Node e) -> Cmd e
appendMany = setupNodes appendDF

||| Sets up the reactive behavior of the given `Node` and
||| appends it to the given element's list of children
export %inline
append : Ref t -> Node e -> Cmd e
append = setupNode appendDF

||| Sets up the reactive behavior of the given `Node`s and
||| prepends them to the given element's list of children
export %inline
prependMany : Ref t -> List (Node e) -> Cmd e
prependMany = setupNodes prependDF

||| Sets up the reactive behavior of the given `Node` and
||| prepends it to the given element's list of children
export %inline
prepend : Ref t -> Node e -> Cmd e
prepend = setupNode prependDF

||| Sets up the reactive behavior of the given `Node`s and
||| replaces the given element.
export %inline
replaceMany : Ref t -> List (Node e) -> Cmd e
replaceMany = setupNodes replaceDF

||| Sets up the reactive behavior of the given `Node` and
||| replaces the given element.
export %inline
replace : Ref t -> Node e -> Cmd e
replace = setupNode replaceDF

||| Sets a custom validity message at the given node.
export %inline
validityMsg : Ref t -> ValidityTag t => String -> Cmd e
validityMsg r s = cmd_ $ setValidityMessage r s

||| Sets or unsets a custom validity message at the given node.
export
validate : Ref t -> ValidityTag t => Either String b -> Cmd e
validate r (Left s)  = validityMsg r s
validate r (Right s) = validityMsg r ""

||| Sets an attribute at the given node.
export
attr : Ref t -> Attribute t e -> Cmd e
attr r a = C $ \h => castElementByRef r >>= \el => setAttribute h el a

||| Sets the `checked` property of the given element
export
checked : Ref Tag.Input -> Bool -> Cmd e
checked r b = C $ \h => castElementByRef r >>= (HTMLInputElement.checked =. b)

||| Sets the `disabled` attribute of the given element
export %inline
disabled : Ref t -> Bool -> Cmd e
disabled r = attr r . disabled

||| Sets the `disabled` attribute of the given element
||| if the given values is a `Left`.
|||
||| This is useful for disabling components such as buttons
||| in the UI in case of invalid user input.
export %inline
disabledE : {0 a,b : _} -> Ref t -> Either a b -> Cmd e
disabledE r = disabled r . isLeft

||| Sets the `disabled` attribute of the given element
||| if the given values is a `Nothing`.
|||
||| This is useful for disabling components such as buttons
||| in the UI in case of invalid user input.
export %inline
disabledM : {0 a : _} -> Ref t -> Maybe a -> Cmd e
disabledM r = disabled r . isNothing

||| Removes the given element from the DOM.
export
remove : Ref t -> Cmd e
remove r = cmd_ (castElementByRef {t = Element} r >>= remove)

||| Sets the `value` attribute of the given element.
export %inline
value : Ref t -> ValueTag t => String -> Cmd e
value r s = cmd_ (setValue r s)

||| Renders a scene at a canvas element
export %inline
renderWithMetrics : Ref Tag.Canvas -> (TextMeasure => CanvasDims -> Scene) -> Cmd e
renderWithMetrics r s = cmd_ (renderWithMetrics r s)

||| Renders a scene at a canvas element
export %inline
renderWithDims : Ref Tag.Canvas -> (CanvasDims -> Scene) -> Cmd e
renderWithDims r s = cmd_ (render r s)

||| Renders a scene at a canvas element
export %inline
render : Ref Tag.Canvas -> Scene -> Cmd e
render r = renderWithDims r . const

||| Adjusts the dimensions of a `HTMLCanvasElement`
export
setCanvasDims : Ref Tag.Canvas -> CanvasDims -> Cmd e
setCanvasDims r d =
  cmd_ $ do
    c <- castElementByRef {t = HTMLCanvasElement} r
    set (height c) (cast d.cheight)
    set (width c) (cast d.cwidth)


||| Focus the given HTMLElemet
export %inline
focus : Ref t -> Cmd e
focus r = cmd_ (castElementByRef {t = HTMLElement} r >>= HTMLOrSVGElement.focus)

||| Blur (lose focus on) the given HTMLElemet
export %inline
blur : Ref t -> Cmd e
blur r = cmd_ (castElementByRef {t = HTMLElement} r >>= HTMLOrSVGElement.blur)

||| Provides a `TextMeasure` utility from the given `Canvas` to run the given
||| command.
export
withMetricsFor : Ref Tag.Canvas -> (TextMeasure => Cmd e) -> Cmd e
withMetricsFor ref c =
  C $ \h => do
    canvas <- castElementByRef {t = HTMLCanvasElement} ref
    ctxt   <- context2D canvas
    run (withMetrics ctxt c) h


%foreign "browser:lambda:(s,w) => document.body.style.cursor = s"
prim__setCursor : String -> PrimIO ()

||| Use the chosen mouse cursor
export
setCursor : Cursor -> Cmd e
setCursor = cmd_ . primIO . prim__setCursor . interpolate
