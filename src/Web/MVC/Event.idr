module Web.MVC.Event

import JS
import Text.HTML.Event
import Web.Dom
import Web.Raw.UIEvents

%default total

%foreign "browser:lambda:x=>x.target.value || x.target.innerHTML || ''"
prim__input : Event -> PrimIO String

%foreign "browser:lambda:x=>x.target.checked?1:0"
prim__checked : Event -> PrimIO Bits8

export
mouseInfo : MouseEvent -> JSIO MouseInfo
mouseInfo e =
  [| MkMouseInfo
     (button e)
     (buttons e)
     (clientX e)
     (clientY e)
     (offsetX e)
     (offsetY e)
     (pageX e)
     (pageY e)
     (screenX e)
     (screenY e)
     (altKey e)
     (ctrlKey e)
     (metaKey e)
     (shiftKey e)
  |]

export
keyInfo : KeyboardEvent -> JSIO KeyInfo
keyInfo e =
  [| MkKeyInfo
     (key e)
     (code e)
     (location e)
     (isComposing e)
     (altKey e)
     (ctrlKey e)
     (metaKey e)
     (shiftKey e)
  |]

export
inputInfo : InputEvent -> JSIO InputInfo
inputInfo e =
  [| MkInputInfo
       (primIO (prim__input $ up e))
       ((1 ==) <$> primIO (prim__checked $ up e)) |]

export
changeInfo : Event -> JSIO InputInfo
changeInfo e =
  [| MkInputInfo
       (primIO (prim__input e))
       ((1 ==) <$> primIO (prim__checked e)) |]
