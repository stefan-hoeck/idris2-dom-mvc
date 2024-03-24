module Text.HTML.Confirm

import Text.HTML
import Text.HTML.Class
import Text.HTML.DomID

%default total

public export
data ConfirmEv : Type -> Type where
  Cancel : ConfirmEv e
  Begin  : ConfirmEv e
  Edited : (event : e) -> ConfirmEv e
  OK     : ConfirmEv e

||| Environment for beginning an editing process.
public export
record ConfirmEnv (i : Type) where
  [noHints]
  constructor CE
  title      : String
  titleCls   : Class
  rowCls     : Class
  okID       : i -> i
  okNode     : {0 e : _} -> i -> e -> Node e
  cancelNode : {0 e : _} -> e -> Node e

parameters {0 i, e : Type}
           {auto cst : Cast i DomID}
           (ce  : ConfirmEnv i)

  export
  dialog : i -> Node e -> List (Node (ConfirmEv e))
  dialog u n =
    [ div [cls Title ce.titleCls] [Text ce.title]
    , Edited <$> n
    , row ce.rowCls [] [ ce.okNode (ce.okID u) OK, ce.cancelNode Cancel]
    ]
