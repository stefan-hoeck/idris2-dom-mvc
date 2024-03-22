module Web.MVC.Controller.Validation

import Data.String
import Text.HTML.Extra
import Web.MVC
import Web.MVC.Controller

%default total

parameters {0      i : Type}
           {auto cst : Cast i DomID}
           {auto env : ValEnv i}

  ||| Sets the associated validation text of a validated widget based on
  ||| its current state.
  export
  validateState : (s -> Either String t) -> i -> s -> Cmd e
  validateState f u = elemChild (env.msgID u) . Text . either id (const "") . f

  ||| Sets the validation message of an `<input>` element based on its
  ||| current value.
  export
  valTextInput : (String -> Either String t) -> i -> Controller String String
  valTextInput rd u = displayEv (validate (inpRef $ env.inputID u) . rd)

  ||| Converts an editor representing an interactive widget to one
  ||| with an associated DOM element for displaying validation text
  ||| messages.
  export
  validated : Editor i s e n -> Editor i s e n
  validated ed =
    E (\u,v => ed.ctrl u v <+> map (validateState ed.stToNew u) get)
      (\u,s => validated u (ed.view u s))
      (\u,s => validateState ed.stToNew u s <+> ed.init u s)
      ed.stToNew
      ed.toState

  export
  checkVal : (String -> Either String t) -> String -> Either String t
  checkVal f s =
    case f s of
      Right v => Right v
      Left  x => if trim s == "" then Left env.mandatory else Left x

  ||| An editor for editing values as strings via `<input>` elements.
  |||
  ||| @cls : CSS class for the input text field we use.
  |||
  ||| @tpe : Type of the input field
  |||
  ||| @rd  : Reading function for converting strings to values of the
  |||        desired type
  |||
  ||| @disp : Function for displaying client-side values.
  |||
  ||| @ini  : Initial (client-side) value
  export %inline
  input :
       (cls     : Class)
    -> (tpe     : InputType)
    -> (stToNew : String -> Either String new)
    -> (disp    : Maybe new -> String)
    -> Editor i String String new
  input c tpe f =
    let g := checkVal f in E (valTextInput g) (vinp c tpe) noInit g
