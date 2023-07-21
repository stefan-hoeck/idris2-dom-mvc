||| Utilities for working with `<select>` elements
module Text.HTML.Select

import Data.List
import Data.Nat
import Text.HTML

%default total

ix : List t -> Nat -> Maybe t
ix []        _     = Nothing
ix (x :: xs) 0     = Just x
ix (x :: xs) (S k) = ix xs k

opt : Eq t => Maybe t -> (t -> String) -> (Nat,t) -> Node e
opt i f (x,v) =
  option [value (show x), selected (i == Just v)] [Text $ f v]

||| Create a `<select>` element displaying the options in the given
||| list.
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
selectFromList vs i disp toEv attrs =
  select
    -- Note: The `change` event handler must be the last attribute
    --       otherwise it might already fire when the node is being
    --       set up.
    (attrs ++ [onChangeMaybe (map toEv . ix vs . cast)])
    (opt i disp <$> zip [0 .. pred (length vs)] vs)