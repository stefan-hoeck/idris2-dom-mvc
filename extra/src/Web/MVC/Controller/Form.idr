module Web.MVC.Controller.Form

import Data.List.Quantifiers.Extra
import Text.HTML
import Web.MVC.Controller

||| A heterogeneous list of editors.
|||
||| These can be used to create UI forms from single - but
||| arbitrarily complex - widgets.
public export
data Editors : (i : Type) -> (sts,evs,news : List Type) -> Type where
  Nil  : Editors i [] [] []
  (::) :
       Editor i s e n
    -> Editors i ss es ns
    -> Editors i (s::ss) (e::es) (n::ns)

||| Converts a list of editors to a list of controllers.
public export
toProgs : Editors i ss es ns -> i -> Progs es ss
toProgs []      vi = []
toProgs (x::xs) vi = x.ctrl vi :: toProgs xs vi

||| Initial commands from a list of editors and a heterogeneous list of
||| initial states.
export
toInit : Editors i ss es ns -> i -> HList ss -> Cmd (HSum es)
toInit []      vi []      = neutral
toInit (x::xs) vi (y::ys) =
  map Here (x.init vi y) <+> map There (toInit xs vi ys)

toNew : Editors i ss es ns -> HList ss -> Either String (HList ns)
toNew []      []      = Right []
toNew (x::xs) (y::ys) = [| stToNew x y :: toNew xs ys |]

toStNothing : Editors i ss es ns -> HList ss
toStNothing []      = []
toStNothing (x::xs) = x.toState Nothing :: toStNothing xs

toSt : Editors i ss es ns -> HList ns -> HList ss
toSt []      []      = []
toSt (x::xs) (y::ys) = x.toState (Just y) :: toSt xs ys

views : Editors i ss es ns -> i -> HList ss -> List (Node $ HSum es)
views []      vi []      = []
views (x::xs) vi (y::ys) =
  Prelude.map Here (view x vi y) :: map (Prelude.map There) (views xs vi ys)

||| Converts a heterogeneous list of editors into a single editor,
||| similar in concept to a form of widgets.
export
form :
     (List (Node $ HSum es) -> Node (HSum es))
  -> Editors i ss es ns
  -> Editor i (HList ss) (HSum es) (HList ns)
form wrapNodes eds =
  E { ctrl    = progs . toProgs eds
    , view    = \vi => wrapNodes . views eds vi
    , init    = toInit eds
    , stToNew = toNew eds
    , toState = maybe (toStNothing eds) (toSt eds)
    }
