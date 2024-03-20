module Web.MVC.Controller

import Data.Either
import Data.List.Quantifiers.Extra
import Monocle
import Text.HTML.DomID
import Web.MVC
import public Control.Monad.State
import public Web.MVC.Cmd

%default total

--------------------------------------------------------------------------------
--          Prog and Controller
--------------------------------------------------------------------------------

infixr 4 <$$>

||| A `Prog` controls some state of type `s` acting on events of
||| type `e`.
|||
||| Based on an event and the current state, the controller produces a new
||| state and a result (typically a command), which usually updates the
||| UI in such a way that it can fire more events.
public export
0 Prog : (e,s,t : Type) -> Type
Prog e s t = e -> State s t

public export
0 Controller : (e,s : Type) -> Type
Controller e s = Prog e s (Cmd e)

export %inline
(<$$>) : (x -> y) -> State s (Cmd x) -> State s (Cmd y)
(<$$>) = map . map

export %inline
map2 : (x -> y) -> State s (Cmd x) -> State s (Cmd y)
map2 = map . map

export %inline
Semigroup (State s $ Cmd e) where x <+> y = [| x <+> y |]

export %inline
Monoid (State s $ Cmd e) where neutral = pure neutral

||| On an event, replaces the current state with the event's
||| value without emitting a command.
export %inline
direct : Monoid t => Prog e e t
direct v = put v $> neutral

||| Same as `direct` but for optional states.
export %inline
directM : Monoid t => Prog e (Maybe e) t
directM v = put (Just v) $> neutral

||| A controller that replaces the state with the current event
||| and display the new state.
export %inline
displayEv : (s -> Cmd t) -> Prog s s (Cmd t)
displayEv disp v = put v $> disp v

||| Update the state with the given function, the run a command
||| derived from the new state.
export %inline
updateDisp : (e -> s -> s) -> (e -> s -> Cmd t) -> Prog e s (Cmd t)
updateDisp f g ev = modify (f ev) >> map (g ev) get

public export
data Progs : (evs : List Type) -> (sts : List Type) -> Type where
  Nil  : Progs Nil Nil
  (::) : Prog e s (Cmd e) -> Progs es ss -> Progs (e::es) (s::ss)

||| A list of controllers can be converted to a controller over
||| a heterogeneous sum as the event type and a heterogeneous list
||| as the state type.
export
progs : Progs es ss -> Controller (HSum es) (HList ss)
progs []      ev = absurd ev
progs (c::cs) (Here x)  = Here  <$$> stL allHead (c x)
progs (c::cs) (There x) = There <$$> stL allTail (progs cs x)

export covering %inline
run : Prog e s (Cmd e) -> (initEv : e) -> (initST : s) -> IO ()
run f = runController (\ev,st => runState st (f ev)) (putStrLn . dispErr)

--------------------------------------------------------------------------------
--          Editor
--------------------------------------------------------------------------------

||| An `Editor` describes how to "pop-up" new interactive DOM elements that
||| typically serve as a form of (validated) user input. It consists of a
||| `Controller` for the interactive element(s), but also describes an initial
||| view to display and command to emit, based on the current state.
|||
||| An editor and its corresponding widgets can be something simple like a
||| text input field or a `<select>` element, or it can be highly complex
||| like a canvas and a group of DOM elements for editing molecules.
|||
||| Since an `Editor` describes a set of DOM elements that can potentially
||| appear several times in a web page (for instance, when editing a list of
||| values), its functions take a `DomID` as additional input, to distinguish
||| between the different versions of the widget that are around.
|||
||| @i   : The ID type used to identify DOM elements
|||
||| @st  : The type used to internally store the current widget state
|||
||| @ev  : The event type the widget fires.
|||
||| @new : The type of data we can extract from the widget's internal state.
public export
record Editor (i,st,ev,new : Type) where
  constructor E
  ||| Update the inner state based on the current event.
  ctrl  : i -> Controller ev st

  ||| Create a widget from a unique identifier and the initial state.
  view  : i -> st -> Node ev

  ||| The initial command use to properly setup the view. This is used
  ||| for initial validation or for drawing molecules to the canvas when
  ||| editing starts. It is invoked after setting up the view.
  init  : i -> st -> Cmd ev

  ||| Try to convert the current state to a value of type `new` we can
  ||| send to the server. This implements client-side validation, therefore,
  ||| it returns an `Either`.
  stToNew : st -> Either String new

  ||| Create an initial state from an optional value of type `new`
  toState : Maybe new -> st

||| Tries to convert the current (optional) state of an editor to
||| an updated value.
export
toNewM : Editor i st ev new -> Maybe st -> Maybe new
toNewM ed = (>>= eitherToMaybe . ed.stToNew)

||| Views the `new` values of an editor through a prism.
export
newP : Prism' t s -> Editor i st ev s -> Editor i st ev t
newP p ed =
  { stToNew := map p.reverseGet . ed.stToNew
  , toState := \x => ed.toState $ x >>= first p
  } ed

||| A dummy `Editor` for uneditable values.
public export
dummy : n -> Editor i () Void n
dummy v =
  E (\_,_ => neutral) (\_,_ => Empty) (\_,_ => neutral) (\_ => Right v) (\_ => ())

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

export %inline
elemChildren : Cast t DomID => t -> List (Node e) -> Cmd e
elemChildren = children . elemRef

export %inline
elemChild : Cast t DomID => t -> Node e -> Cmd e
elemChild = child . elemRef

export %inline
elemAppend : Cast t DomID => t -> Node e -> Cmd e
elemAppend = append . elemRef

export %inline
elemPrepend : Cast t DomID => t -> Node e -> Cmd e
elemPrepend = prepend . elemRef

export %inline
clearElem : Cast t DomID => t -> Cmd e
clearElem v = elemChildren v []

export %inline
removeElem : Cast t DomID => t -> Cmd e
removeElem = remove . elemRef

export %inline
btnAttr : Cast t DomID => t -> Attribute Tag.Button e -> Cmd e
btnAttr v = attr (btnRef v)
