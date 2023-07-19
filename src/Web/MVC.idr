module Web.MVC

import Data.IORef
import Data.List.Quantifiers.Extra

import public JS
import public Web.MVC.Cmd
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

||| Runs (a part of) an interactive web page firing events of type
||| `e` and holding state of type `s`.
|||
||| The controller is used for updating and displaying the new
||| application state upon an event, an effectful computation that
||| might include the registering of new events in the UI, for
||| which the controller requires and auto implicit argument of
||| type `Handler e`.
export covering
runMVC :
     {0 e,s  : Type}
  -> (initEv : e)
  -> (initST : s)
  -> (ctrl   : Handler e => Controller s e)
  -> JSIO ()
runMVC initEv initST ctrl = do
  ref <- newIORef initST

  let covering handler : Handler e
      handler = H $ \ev => do
        stOld <- readIORef ref
        stNew <- ctrl @{handler} ev stOld
        writeIORef ref stNew

  handler.handle_ initEv
