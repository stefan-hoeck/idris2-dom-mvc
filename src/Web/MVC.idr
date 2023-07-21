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

||| Run (a part of) an interactive web page firing events of type
||| `e` and holding state of type `s`.
|||
||| The controller is used for updating and displaying the new
||| application state upon an event, an effectful computation that
||| might include the registering of new events in the UI, for
||| which the controller requires and auto implicit argument of
||| type `Handler e`.
|||
||| Important note: Passing the event handler explicitly to the
||| controller might lead to it being  invoke with an event from
||| within an event handling step. This could potentially lead to
||| an infinite loop and is the reason why this function is
||| not total. So, make sure to only register the event handler
||| as an event listener or similar at components that will
||| invoke it asynchrounously.
export covering
runController :
     {0 e,s  : Type}
  -> (initEv : e)
  -> (initST : s)
  -> (ctrl   : Handler e => Controller s e)
  -> JSIO ()
runController initEv initST ctrl = do
  ref <- newIORef initST

  let covering handler : Handler e
      handler = H $ \ev => do
        stOld <- readIORef ref
        stNew <- ctrl @{handler} ev stOld
        writeIORef ref stNew

  handler.handle_ initEv

||| Run an interactive web page firing events of type
||| `e` and holding state of type `s`.
|||
||| This is a simplified version of `runController` for applications
||| that do not require additional side effects when updating the
||| state.
|||
||| More complex setups might include random number generation or
||| setting up and breaking down scarce resources such as animations.
||| These should be handled with `runController`.
|||
||| @ update  : Update the state according to the current event.
||| @ display : Update the view according to the current event and
|||             *updated* state.
||| @initEv   : Event used for initializing the user interface
||| @initST   : Initial application state.
export covering %inline
runMVC :
     {0 e,s  : Type}
  -> (update  : e -> s -> s)
  -> (display : e -> s -> Cmds e)
  -> (initEv  : e)
  -> (initST  : s)
  -> JSIO ()
runMVC update display ev st = runController ev st (runDOM update display)
