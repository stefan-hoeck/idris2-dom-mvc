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

%foreign "browser:lambda:x=>x.target.files || []"
prim__files : Event -> PrimIO FileList

%foreign "browser:lambda:x=>x.length"
prim__length : FileList -> PrimIO Bits32

%foreign "browser:lambda:(x,y)=>x[y]"
prim__item : FileList -> Bits32 -> File

files : Event -> JSIO (List File)
files e = do
  fs <- primIO (prim__files e)
  l  <- primIO (prim__length fs)
  pure $ case l of
    0 => []
    x => prim__item fs <$> [0..x-1]

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
changeInfo : Event -> JSIO InputInfo
changeInfo e =
  [| MkInputInfo
       (primIO (prim__input e))
       (files e)
       ((1 ==) <$> primIO (prim__checked e)) |]

export
inputInfo : InputEvent -> JSIO InputInfo
inputInfo e = changeInfo $ up e

export
scrollInfo : Event -> JSIO ScrollInfo
scrollInfo e = do
  Just et <- target e          | Nothing => pure $ MkScrollInfo 0 0 0
  case castTo Element et of
    Nothing => pure $ MkScrollInfo 0 0 0
    Just x =>
      [| MkScrollInfo (get x scrollTop) (scrollHeight x) (clientHeight x) |]

export
wheelInfo : WheelEvent -> JSIO WheelInfo
wheelInfo e =
  [| MkWheelInfo
     (deltaMode e)
     (deltaX e)
     (deltaY e)
     (deltaZ e) |]
