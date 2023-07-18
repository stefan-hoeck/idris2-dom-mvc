module Web.MVC

import Data.IORef
import Data.List.Quantifiers.Extra

import public JS
import public Web.MVC.DOMUpdate
import public Web.MVC.Reactimate
import public Text.HTML

||| A controller is an effectful computation for
||| updating a displaying some application state based
||| on an event type e.
|||
||| When dealing with a heterogeneous sum of possible events,
||| as is encouraged here for writing applications with several
||| more or less unrelated event sources, it is convenient
||| to wrap one controller for handling each event in a
||| heterogeneous list from `Data.List.Quantifiers`. Therefore,
||| the state type comes first in this alias, and the event
||| type comes second, even though in actual controller function
||| it's the other way round.
public export
0 Controller : Type -> Type -> Type
Controller s e = e -> s -> JSIO s

||| Given a heterogeneous list of controllers, we can react
||| on the events in a heterogeneous sum.
export
controlMany : All (Controller s) es -> Controller s (HSum es)
controlMany cs evs s = collapse' $ hzipWith (\f,e => f e s) cs evs

export covering
runMVC :
     {0 e,s  : Type}
  -> (initEv : e)
  -> (initST : s)
  -> (modST  : Handler e -> Controller s e)
  -> JSIO ()
runMVC initEv initST modST = do
  ref <- newIORef initST

  let covering handler : Handler e
      handler ev = do
        stOld <- readIORef ref
        stNew <- modST handler ev stOld
        writeIORef ref stNew

  handler initEv
