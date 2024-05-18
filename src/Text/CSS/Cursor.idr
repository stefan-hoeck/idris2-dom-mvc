module Text.CSS.Cursor

%default total


--------------------------------------------------------------------------------
--          Cursor Types
--------------------------------------------------------------------------------

||| All standard mouse cursors.
|||
||| For using custom icons, use type `Url` linking to an
||| image and choose the offset for the mouse position.
public export
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
