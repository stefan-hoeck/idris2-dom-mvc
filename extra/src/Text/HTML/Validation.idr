module Text.HTML.Validation

import Text.HTML
import Text.HTML.Class
import Text.HTML.DomID

%default total

||| Environment for validating editable componends
public export
record ValEnv (i : Type) where
  [noHints]
  constructor VE

  ||| DOM ID for `<input>` fields based on a parent ID
  inputID   : i -> i

  ||| DOM ID for validation message elements based on a parent ID
  msgID     : i -> i

  ||| Class used for the component display the validated widget
  widgetCls : Class

  ||| Class used for the component display the validation message
  msgCls    : Class

  ||| String for mandatory fields. This is used with validated `<input>`
  ||| fields, when they contain only empty (invalid) input.
  mandatory : String

parameters {0      i : Type}
           {auto cst : Cast i DomID}
           {auto env : ValEnv i}

  ||| Pairs a widget with an element where validation messages can be
  ||| displayed.
  export
  validated : i -> Node e -> Node e
  validated _ Empty = Empty
  validated u n     =
    cell env.widgetCls [] [n, cell env.msgCls [ref (env.msgID u)] []]

  ||| A validated text field.
  export
  vinp : Class -> (tpe : InputType) -> i -> String -> Node String
  vinp c tpe u v = inp c id [value v, ref $ env.inputID u, type tpe]
