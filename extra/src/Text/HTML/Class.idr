module Text.HTML.Class

import Derive.Prelude
import Text.HTML
import Text.HTML.Select
import Text.HTML.DomID

%default total
%language ElabReflection

||| A CSS class
public export
record Class where
  constructor C
  value : String

%runElab derive "Class" [Show,Eq,Ord,FromString]

--------------------------------------------------------------------------------
--          Utility Classes
--------------------------------------------------------------------------------

||| A UI widget (typically, an interactive element such as a button
||| or text input field)
export
Widget : SnocList Class
Widget = [<"widget"]

||| A clickable button with some text.
export
Btn : SnocList Class
Btn = Widget :< "button"

||| A larger UI component
export
Comp : SnocList Class
Comp = [<"comp"]

||| A cell in a table
export
Cell : SnocList Class
Cell = [<"cell"]

||| A clickable icon
export
Icon : SnocList Class
Icon = [<"icon"]

||| A text input field
export
Inp : SnocList Class
Inp = Widget :< "input"

||| A select element
export
Sel : SnocList Class
Sel = Widget :< "select"

||| A text typically linked to some input element
export
Lbl : SnocList Class
Lbl = [<"label"]

||| A larger view displaying a list of values
export
Lst : SnocList Class
Lst = [<"list"]

||| A single row in a list of values
export
Row : SnocList Class
Row = [<"row"]

||| A title in a UI component
export
Title : SnocList Class
Title = [<"title"]

||| A collapsed UI element (hiding some details)
export
collapsed : Class
collapsed = "collapsed"

||| An expanded UI element
export
expanded : Class
expanded = "expanded"

--------------------------------------------------------------------------------
--          UI Elements
--------------------------------------------------------------------------------

clsStrings : SnocList Class -> List String -> List String
clsStrings [<]       ss = ss
clsStrings (sx :< x) ss = clsStrings sx (x.value :: ss)

export
cls : SnocList Class -> Class -> Attribute t e
cls sv v = classes $ clsStrings sv [v.value]

export %inline
cls1 : Class -> Attribute t e
cls1 = cls [<]

||| Creates a text label for a probably editable field
|||
||| @ uid   : ID used in "for" attribute
||| @ class : CSS class to use
||| @ txt   : actual textual content
export
lbl : Cast i DomID => Class -> i -> String -> Node e
lbl c vi txt = label [cls Lbl c, forID vi] [Text txt]

||| A cell in a table
export %inline
cell : Class -> List (Attribute Div e) -> List (Node e) -> Node e
cell v as = div (cls Cell v :: as)

||| A list of elements.
export %inline
list : Class -> List (Attribute Ul e) -> List (Node e) -> Node e
list v as = ul (cls Lst v :: as)

||| A single row in a list of elements.
export %inline
row : Class -> List (Attribute Div e) -> List (Node e) -> Node e
row v as = div (cls Row v :: as)

||| A clickable button in the UI firing the given event on a left click.
export %inline
btn : Class -> e -> String -> List (Attribute Tag.Button e) -> Node e
btn v ev txt as = button (cls Btn v :: onClick ev :: as) [Text txt]

||| A clickable icon in the UI firing the given event on a left click.
export %inline
icon : Class -> e -> List (Attribute Tag.Button e) -> Node e
icon v ev as = button (cls Icon v :: onClick ev :: as) []

||| A clickable icon with predefined class.
|||
||| This takes a `DomID` as additional argument since we want
||| to be able to disable it in case of invalid input.
export %inline
iok : Cast i DomID => i -> e -> Node e
iok u ev = icon "ok" ev [ref u]

||| A clickable icon with predefined class.
export %inline
iadd : e -> Node e
iadd v = icon "add" v []

||| A clickable icon with predefined class.
export %inline
icancel : e -> Node e
icancel v = icon "cancel" v []

||| A clickable icon with predefined class.
export %inline
idelete : e -> Node e
idelete v = icon "delete" v []

--------------------------------------------------------------------------------
--          Editing
--------------------------------------------------------------------------------

||| An `<input>` element of the given class.
export %inline
inp : Class -> (String -> e) -> List (Attribute Tag.Input e) -> Node e
inp v f as = input (cls Inp v :: onInput f :: as) []

||| A select element displaying the values of type `v`
||| shown in the given list.
|||
||| It fires events of type `t`, and uses two functions, one for
||| converting elements to events and one for displaying elements.
export %inline
sel : Eq t => (v -> t) -> (v -> String) -> List v -> Class -> Maybe t -> Node t
sel f g vs c i = selectFromListBy vs ((i ==) . Just . f) g f [cls Sel c]
