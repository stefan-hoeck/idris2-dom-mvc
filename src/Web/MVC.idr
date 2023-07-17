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
  -> (modST   : e -> s -> s)
  -> (display : (e -> JSIO ()) -> e -> s -> JSIO ())
  -> JSIO ()
runMVC initEv initST modST display = do
  ref <- newIORef initST

  let covering handler : e -> JSIO ()
      handler ev = do
        modifyIORef ref (modST ev)
        st <- readIORef ref
        display handler ev st

  handler initEv
