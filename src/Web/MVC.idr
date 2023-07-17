module Web.MVC

import JS
import Data.IORef
import public Web.MVC.ElemRef
import public Web.MVC.Event
import public Web.MVC.Reactimate
import public Text.HTML

export covering
runMVC :
     {0 e,s : Type}
  -> (initEv  : e)
  -> (initST  : s)
  -> (modST   : (e -> JSIO ()) -> e -> s -> JSIO s)
  -> JSIO ()
runMVC initEv initST modST = do
  ref <- newIORef initST

  let covering handler : e -> JSIO ()
      handler ev = do
        stOld <- readIORef ref
        stNew <- modST handler ev stOld
        writeIORef ref stNew

  handler initEv
