module Web.MVC

import Data.IORef

import public JS
import public Web.MVC.DOMUpdate
import public Web.MVC.Reactimate
import public Text.HTML

export covering
runMVC :
     {0 e,s : Type}
  -> (initEv  : e)
  -> (initST  : s)
  -> (modST   : Handler e -> e -> s -> JSIO s)
  -> JSIO ()
runMVC initEv initST modST = do
  ref <- newIORef initST

  let covering handler : Handler e
      handler ev = do
        stOld <- readIORef ref
        stNew <- modST handler ev stOld
        writeIORef ref stNew

  handler initEv
