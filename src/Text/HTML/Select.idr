||| Utilities for working with `<select>` elements
module Text.HTML.Select

import Data.List
import Data.Nat
import Text.HTML

%default total

public export
data SelectEntry : (t : Type) -> Type where
  Title : String    -> SelectEntry t
  Entry : (val : t) -> SelectEntry t

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

ix : List (SelectEntry t) -> Nat -> Maybe t
ix []              _     = Nothing
ix (Entry v :: xs) 0     = Just v
ix (Title s :: xs) 0     = Nothing
ix (x :: xs)       (S k) = ix xs k

app : SnocList (a,List b) -> SnocList b -> a -> SnocList (a,List b)
app sx [<] x = sx
app sx sy  x = sx :< (x, sy <>> [])

groups :
     SnocList (String,List (Nat,t))
  -> SnocList (Nat,t)
  -> (lbl : String)
  -> Nat
  -> List (SelectEntry t)
  -> List (String,List (Nat,t))
groups sg sp l n []              = app sg sp l <>> []
groups sg sp l n (Title s :: xs) = groups (app sg sp l) [<] s (S n) xs
groups sg sp l n (Entry v :: xs) = groups sg (sp :< (n,v))  l (S n) xs

export
selectEntries :
     (entries : List (SelectEntry t))
  -> (sel     : t -> Bool)
  -> (display : t -> String)
  -> (toEvent : t -> e)
  -> (attrs   : List (Attribute Select e))
  -> Node e
selectEntries es sel disp toEv attrs =
  select
    -- Note: The `change` event handler must be the last attribute
    --       otherwise it might already fire when the node is being
    --       set up.
    (attrs ++ [onChangeMaybe (map toEv . ix es . cast)])
    (groups [<] [<] "" 0 es >>= grp)
  where
    opt : (Nat,t) -> Node e
    opt (x,v) = option [value (show x), selected (sel v)] [Text $ disp v]

    grp : (String,List (Nat,t)) -> List (Node e)
    grp ("",ps) = map opt ps
    grp (s,ps)  = [optgroup [label s] $ map opt ps]

||| Create a `<select>` element displaying the options in the given
||| list.
|||
||| @values  : the list of options
||| @sel     : true if the given item in the list should be selected
||| @display : how to display an option at the UI
||| @toEvent : how to convert an option to an event
||| @attrs   : additional attributes
export %inline
selectFromListBy :
     (values  : List t)
  -> (sel     : t -> Bool)
  -> (display : t -> String)
  -> (toEvent : t -> e)
  -> (attrs   : List (Attribute Select e))
  -> Node e
selectFromListBy = selectEntries . map Entry

||| Like `selectFromListBy` but uses an optional initial value
||| to determine the initially selected value.
|||
||| @values  : the list of options
||| @init    : the initially selected option (if any)
||| @display : how to display an option at the UI
||| @toEvent : how to convert an option to an event
||| @attrs   : additional attributes
export
selectFromList :
     {auto eq : Eq t}
  -> (values  : List t)
  -> (init    : Maybe t)
  -> (display : t -> String)
  -> (toEvent : t -> e)
  -> (attrs   : List (Attribute Select e))
  -> Node e
selectFromList vs i = selectFromListBy vs ((i ==) . Just)
