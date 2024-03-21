module Web.MVC.Controller.List

import Monocle
import Web.MVC.Controller
import Text.HTML
import Text.HTML.Class
import Text.HTML.DomID

%default total

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

zipWithIndex : List t -> List (Nat,t)
zipWithIndex vs = fst (foldl (\(x,n),v => (x:<(n,v),S n)) ([<],Z) vs) <>> []

del : Nat -> List (Nat,t) -> List (Nat,t)
del n = filter ((n /=) . fst)

nextID : List (Nat,t) -> Nat
nextID []        = 0
nextID (x :: xs) = S $ foldl (\x => max x . fst) (fst x) xs

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| An event fired when editing a list of items
public export
data ListEv : Type -> Type where
  Add    : ListEv e
  Del    : (ix  : Nat) -> ListEv e
  Change : (ix : Nat) -> (ev : e) -> ListEv e

||| Whether to append or prepend items added to the list
public export
data ListMode = Append | Prepend

insert : ListMode -> Nat -> s -> List (Nat,s) -> List (Nat,s)
insert Append  n v vs = vs ++ [(n,v)]
insert Prepend n v vs = (n,v) :: vs

insertCmd : Cast i DomID => ListMode -> i -> Node e -> Cmd e
insertCmd Append  = elemAppend
insertCmd Prepend = elemPrepend

||| Environment for setting up the UI elements
public export
record ListEnv (i : Type) where
  constructor LI
  ||| ID for a single row
  rowID   : i -> Nat -> i

  ||| ID for the whole list base on a parent ID
  listID  : i -> i

  ||| Class suffix
  cls     : Class

  ||| Icon used for deleting a row
  delIcon : {0 e : _} -> e -> Node e

  ||| Icon used for adding a row
  addIcon : {0 e : _} -> e -> Node e

  ||| Whether to append or prepend new values
  mode    : ListMode

--------------------------------------------------------------------------------
-- Editors
--------------------------------------------------------------------------------

parameters {0 i,s,e,t : Type}
           {auto cst  : Cast i DomID}
           (li        : ListEnv i)

  ||| Row for editing a single value in a list of values.
  export
  listRow : (i -> s -> Node e) -> i -> (Nat,s) -> Node (ListEv e)
  listRow f u (n,x) =
    let lid := li.rowID u n
     in div [cls (Row :< "list-edit") li.cls, ref lid]
          [Change n <$> f lid x, li.delIcon (Del n)]

  ||| A widget used for editing lists of values, using another
  ||| widget for each individual value.
  export
  values : (i -> s -> Node e) -> i -> List (Nat,s) -> Node (ListEv e)
  values f u ps =
    div [cls (Cell :< "list-edit") li.cls ]
       [ div [cls (Row :< "list-add") li.cls] [li.addIcon Add]
       , div [cls (Lst :< "list-edit") li.cls, ref $ li.listID u]
           (listRow f u <$> ps)
       ]

  ||| An editor for lists of values, where values can be dynamically
  ||| added or removed.
  export
  list : Editor i s e t -> Editor i (List (Nat,s)) (ListEv e) (List t)
  list ed =
    E adj
      (values ed.view)
      (\u => batch . map (\(n,v) => init u n v))
      (traverse (ed.stToNew . snd))
      (maybe [] (zipWithIndex . map (ed.toState . Just)))

    where
      row : i -> Nat -> s -> Node (ListEv e)
      row u n v = listRow ed.view u (n,v)

      ctrl : i -> Nat -> Prog e s (Cmd $ ListEv e)
      ctrl u n v = Change n <$$> ed.ctrl (li.rowID u n) v

      init : i -> Nat -> s -> Cmd (ListEv e)
      init u n v = Change n <$> ed.init (li.rowID u n) v

      adj : i -> Controller (ListEv e) (List (Nat,s))
      adj u Add = do
        n <- map nextID get
        let s   := ed.toState Nothing
        modify (insert li.mode n s)
        pure $ insertCmd li.mode (li.listID u) (row u n s) <+> init u n s
      adj u (Del n) = modify (del n) $> (removeElem $ li.rowID u n)
      adj u (Change n v) = stO (eqFirst n fst .> snd) (ctrl u n v)
