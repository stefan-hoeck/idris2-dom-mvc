module Web.MVC.Controller.Confirm

import Monocle
import Text.HTML
import Text.HTML.Class
import Text.HTML.Confirm
import Text.HTML.DomID
import Web.MVC
import Web.MVC.Controller

%default total

parameters {0 i,s,e,t,state : Type}
           {auto cst : Cast i DomID}
           (ce       : ConfirmEnv i)
           (ed       : Editor i s e t)
           (valL     : Lens' state s)
           (get      : state -> Maybe t)

  -- In `Cancel` case, do we need to cleanup?
  -- In `OK` case, do we cleanup and overwrite the state?
  upd : ConfirmEv e -> state -> state
  upd Begin  st = setL valL (ed.toState $ get st) st
  upd _      st = st

  disp : i -> ConfirmEv e -> state -> Cmd (ConfirmEv e)
  disp u Begin  st =
    let v := ed.toState $ get st
     in batch
          [ elemChildren u (dialog ce u $ ed.view u v)
          , Edited <$> ed.init u v
          , disabledE (elemRef $ ce.okID u) (ed.stToNew v)
          ]
  disp u Cancel st = clearElem u
  disp u _      st = neutral

  export
  confirm : i -> ConfirmEv e -> State state (Cmd $ ConfirmEv e)
  confirm u (Edited ev) = do
    c1 <- Edited <$$> stL valL (ed.ctrl u ev)
    c2 <- disabledE (elemRef $ ce.okID u) . ed.stToNew <$> getST valL
    pure $ c1 <+> c2
  confirm u ev          = updateDisp upd (disp u) ev

--  editCmd : UILocal => ExpEv e f -> (Cmd $ ExpEv e f)
--  editCmd CancelAdd = clearElem AddColumn
--  editCmd (DoAdd x) = clearElem AddColumn
--  editCmd (AddAfter x) =
--    let node := ee.colEd.view AddColRow (ee.colEd.toState Nothing)
--        cmd  := ee.colEd.init AddColRow (ee.colEd.toState Nothing)
--     in elemChild AddColumn (addColumn x node) <+> map Edited cmd
--  editCmd _         = neutral
--
--  -- adjust user settings based on the current events
--  sets : ExpEv e f -> ExpST f -> UserSettings q f a -> UserSettings q f a
--  sets (SortBy x b) st = over lastQueryL (setL sortL x .  setL reverseL b)
--  sets (Hide x)     st = {substanceColumns $= filter (/= x)}
--  sets (DoAdd x)    st = maybe id (addColumnAfter x) st.column
--  sets  _           st = id
--
--  -- display more compounds
--  disp : ExpEv e f -> ExpST f -> Cmd event
--  disp evt st =
--    case act evt st of
--      DispMore => ee.dispMore st.displayed st.dispCount
--      LoadMore => ee.loadMore st.loaded NumLoad
--      NoAct    => neutral
--
--  -- adjust dispCount *after* actually displaying new compounds
--  adj : ExpEv e f -> ExpST f -> ExpST f
--  adj ev st =
--    case act ev st of
--      LoadMore => {ready := False} st
--      DispMore => {displayed $= (+ st.dispCount)} st
--      NoAct    => st
--
--  led : ListEditor UIID f e f
--  led = list (listEnv "field" Append) ee.expEd
--
--  ||| Controller for the explorer and its events
--  |||
--  ||| This updates the explorer's internal state but also the
--  ||| current user settings, displayed columns, and sorting
--  ||| column of queries.
--  export
--  expC : UILocal => Prog (ExpEv e f) state (Cmd event)
--  expC StopExport  = setST (ee.exp |> colsL) [] $> clearElem ExportCols
--  expC StartExport = do
--    cs <- getST (ee.sets |> exportFieldsL)
--    setST (ee.exp |> colsL) (led.toState $ Just cs)
--    pure $ ee.toEv <$> elemChildren ExportCols (exportFields led ee.encode cs)
--  expC (Fields f) = do
--    c  <- stL (ee.exp |> colsL) (led.ctrl ExportCols f)
--    vs <- map snd <$> getST (ee.exp |> colsL)
--    pure $
--      map (ee.toEv . Fields) c <+>
--      attr (btnRef ExportOK) (href $ exportRef (commaSep ee.encode vs))
--  expC DoExport   = do
--    vs <- map snd <$> getST (ee.exp |> colsL)
--    setST (ee.sets |> exportFieldsL) vs $> neutral
--  expC (Edited x) =
--    ee.toEv . Edited <$$> stO (ee.exp .> columnL .> just) (ee.colEd.ctrl AddColRow x)
--  expC ev = do
--    overST ee.exp (plain ev)
--    withST ee.exp (overST ee.sets . sets ev)
--    c1 <- disp ev <$> getST ee.exp
--    overST ee.exp (adj ev)
--    pure $ c1 <+> (ee.toEv <$> editCmd ev)
