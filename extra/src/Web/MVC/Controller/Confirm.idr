module Web.MVC.Controller.Confirm

import Monocle
import Text.HTML
import Text.HTML.Class
import Text.HTML.Confirm
import Text.HTML.DomID
import Web.MVC
import Web.MVC.Controller

%default total

public export
record ConfirmEnv (i,s,e,t,event,state : Type) where
  constructor CE
  conf : ConfirmConfig i
  ed   : Editor i s e t
  val  : Lens' state s
  ini  : state -> Maybe t
  toEv : ConfirmEv e -> event
  onOK : t -> State state (Cmd event)

parameters {0 i,s,e,t,event,state : Type}
           {auto cst : Cast i DomID}
           (ce       : ConfirmEnv i s e t event state)

  export %inline
  curNew : state -> Either String t
  curNew = ce.ed.stToNew . ce.val.get_

  upd : ConfirmEv e -> state -> state
  upd Begin  st = setL ce.val (ce.ed.toState $ ce.ini st) st
  upd _      st = st

  disp : i -> ConfirmEv e -> state -> Cmd (ConfirmEv e)
  disp u Begin  st =
    let v := ce.ed.toState $ ce.ini st
     in batch
          [ elemChild u (dialog ce.conf u $ ce.ed.view u v)
          , Edited <$> ce.ed.init u v
          , disabledE (elemRef $ ce.conf.okID u) (ce.ed.stToNew v)
          ]
  disp u Cancel st = clearElem u
  disp u _      st = neutral

  export
  confirm : i -> ConfirmEv e -> State state (Cmd event)
  confirm u (Edited ev) = do
    c1 <- ce.toEv . Edited <$$> stL ce.val (ce.ed.ctrl u ev)
    c2 <- disabledE (elemRef $ ce.conf.okID u) . ce.ed.stToNew <$> getST ce.val
    pure $ c1 <+> c2
  confirm u OK          = do
    Right vt <- ce.ed.stToNew <$> getST ce.val | Left _ => neutral
    ce.onOK vt
  confirm u ev          = ce.toEv <$$> updateDisp upd (disp u) ev
