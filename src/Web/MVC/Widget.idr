module Web.MVC.Widget

import Web.MVC

%default total

--------------------------------------------------------------------------------
--          Widgets
--------------------------------------------------------------------------------


||| A `Widget` is a standalone UI element that manages its own
||| state. It packages up all aspects of an MVC component into a
||| single piece of data that can be passed around and transformed,
||| before finally turned into a runnable program with `runWidget`.
|||
||| See the various parameters of `Web.MVC.runMVC` for further
||| explanation.
public export
record Widget where
  constructor MkWidget

  ||| The internal state of the widget (model)
  0 St : Type
  ||| Event type
  0 Ev : Type
  ||| Initial state
  init : St
  ||| Given the initial state, set up the  UI
  setup : St -> Cmd Ev
  ||| Update the state based on the latest event
  update : Ev -> St -> St
  ||| Update the UI based on the latest event and the current state
  display : Ev -> St -> Cmd Ev

||| `w1 <+> w2` is the independent composition of widgets `w1` and
||| `w2`, with the product state and the sum events.
public export
Semigroup Widget where
  w1 <+> w2 = MkWidget
    { St = (w1.St, w2.St)
    , Ev = Either w1.Ev w2.Ev
    , init = (w1.init, w2.init)
    , setup = \(s1, s2) => batch
        [ Left <$> w1.setup s1
        , Right <$> w2.setup s2
        ]
    , update = \ev, (s1, s2) => case ev of
        Left  ev1 => (w1.update ev1 s1, s2)
        Right ev2 => (s1, w2.update ev2 s2)
    , display = \ev, (s1, s2) => case ev of
        Left ev  => Left <$> w1.display ev s1
        Right ev => Right <$> w2.display ev s2
    }

||| `neutral` is the trivial widget with trivial state and no events
public export
Monoid Widget where
  neutral = MkWidget
    { St = ()
    , Ev = Void
    , init = ()
    , setup = neutral
    , update = absurd
    , display = \_, _ => neutral
    }

||| Run a `Widget`. This is basically `runMVC` with the arguments
||| constructed from the fields of `Widget`. Since this function
||| produces no result, there's no going back from here: all `Widget`
||| composition and transformation must be done beforehand.
export
covering
runWidget : (JSErr -> IO ()) -> Widget -> IO ()
runWidget onError w = runMVC update display onError Nothing w.init
  where
    update : Maybe w.Ev -> w.St -> w.St
    update Nothing = id
    update (Just ev) = w.update ev

    display : Maybe w.Ev -> w.St -> Cmd (Maybe w.Ev)
    display ev s = Just <$> maybe w.setup w.display ev s
