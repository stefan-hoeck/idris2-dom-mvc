module Text.HTML.Event

import Data.Contravariant
import Data.Maybe
import Web.Internal.FileTypes

%default total

--------------------------------------------------------------------------------
--          Event Info Types
--------------------------------------------------------------------------------

public export
record WheelInfo where
  constructor MkWheelInfo
  deltaMode : Bits32
  deltaX    : Double
  deltaY    : Double
  deltaZ    : Double

public export
record MouseInfo where
  constructor MkMouseInfo
  -- buttons
  button  : Int16
  buttons : Bits16

  -- coordinates
  clientX : Double
  clientY : Double
  offsetX : Double
  offsetY : Double
  pageX   : Double
  pageY   : Double
  screenX : Double
  screenY : Double

  -- keys
  alt     : Bool
  ctrl    : Bool
  meta    : Bool
  shift   : Bool

public export
record InputInfo where
  constructor MkInputInfo
  value   : String
  files   : List File
  checked : Bool

public export
record KeyInfo where
  constructor MkKeyInfo
  key         : String
  code        : String
  location    : Bits32
  isComposing : Bool

  -- control keys
  alt         : Bool
  ctrl        : Bool
  meta        : Bool
  shift       : Bool

public export
record ScrollInfo where
  constructor MkScrollInfo
  scrollTop    : Double
  scrollHeight : Int32
  clientHeight : Int32


--------------------------------------------------------------------------------
--          Events
--------------------------------------------------------------------------------

public export
data DOMEvent : Type -> Type where
  -- Mouse clicks
  Click      : (MouseInfo -> Maybe a) -> DOMEvent a
  DblClick   : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseDown  : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseUp    : (MouseInfo -> Maybe a) -> DOMEvent a

  -- Mouse movement
  MouseEnter : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseLeave : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseOver  : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseOut   : (MouseInfo -> Maybe a) -> DOMEvent a
  MouseMove  : (MouseInfo -> Maybe a) -> DOMEvent a

  -- Focus
  Blur       : a -> DOMEvent a
  Focus      : a -> DOMEvent a

  -- Keyboard
  KeyDown    : (KeyInfo -> Maybe a) -> DOMEvent a
  KeyUp      : (KeyInfo -> Maybe a) -> DOMEvent a

  -- Input
  Change     : (InputInfo -> Maybe a) -> DOMEvent a
  Input      : (InputInfo -> Maybe a) -> DOMEvent a

  -- Routing
  HashChange : a -> DOMEvent a

  -- Scrolling
  Scroll     : (ScrollInfo -> Maybe a) -> DOMEvent a

  -- Wheel
  Wheel      : (WheelInfo -> Maybe a)  -> DOMEvent a

export
Functor DOMEvent where
  map f (Click g)      = Click (map f . g)
  map f (DblClick g)   = DblClick (map f . g)
  map f (MouseDown g)  = MouseDown (map f . g)
  map f (MouseUp g)    = MouseUp (map f . g)
  map f (MouseEnter g) = MouseEnter (map f . g)
  map f (MouseLeave g) = MouseLeave (map f . g)
  map f (MouseOver g)  = MouseOver (map f . g)
  map f (MouseOut g)   = MouseOut (map f . g)
  map f (MouseMove g)  = MouseMove (map f . g)
  map f (Blur x)       = Blur (f x)
  map f (Focus x)      = Focus (f x)
  map f (KeyDown g)    = KeyDown (map f . g)
  map f (KeyUp g)      = KeyUp (map f . g)
  map f (Change g)     = Change (map f . g)
  map f (Input g)      = Input (map f . g)
  map f (HashChange x) = HashChange (f x)
  map f (Scroll g)     = Scroll (map f . g)
  map f (Wheel g)      = Wheel (map f . g)
