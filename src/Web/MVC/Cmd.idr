module Web.MVC.Cmd

import Control.Monad.Either.Extra
import JS

%default total

--------------------------------------------------------------------------------
--          Commands
--------------------------------------------------------------------------------

||| A `Cmd` (abbreviation of "command") is a (typically effectful) computation
||| which might send an arbitrary number of events of type `e` to an event
||| handler synchronously or asynchronously.
|||
||| Commands are used as the primary means of setting up (interactive) UI
||| components and running effectful computations.
|||
||| Module `Web.MVC.View` provides various commands for creating, modifying
||| and deleting interactive DOM elements.
|||
||| Module `Web.MVC.Animate` has commands for firing events at regular
||| intervals and for running animations.
|||
||| Module `Web.MVC.Http` has commands for sending requests to and
||| firing events upon receiving responses from HTTP servers.
public export
record Cmd (e : Type) where
  constructor C
  run : (handler : e -> JSIO ()) -> JSIO ()

export %inline
Functor Cmd where
  map f (C run) = C $ run . (. f)

export
Semigroup (Cmd e) where
  C f <+> C g = C $ \h => f h >> g h

export %inline
Monoid (Cmd e) where
  neutral = C . const $ pure ()

||| Wraps a batch of commands in a single command by
||| installing each command sequentially.
|||
||| This function is stack safe.
export
batch : List (Cmd e) -> Cmd e
batch cs = C $ \h => traverseList_ (`run` h) cs

||| Wrap an effectful computation in a command.
|||
||| The produced result is fired synchronously.
export %inline
cmd : JSIO e -> Cmd e
cmd v = C (v >>=)

||| Wrap an effectful computation in a command.
|||
||| The produced result is fired synchronously.
export %inline
liftIO : IO e -> Cmd e
liftIO = cmd . liftIO

||| Wrap an effectful computation in a command.
|||
||| This will never fire an event.
export %inline
cmd_ : JSIO () -> Cmd e
cmd_ v = C $ const v

||| Wrap an effectful computation in a command.
|||
||| This will never fire an event.
export %inline
liftIO_ : IO () -> Cmd e
liftIO_ = cmd_ . liftIO

||| Fires the given event once, synchronously.
export %inline
pure : e -> Cmd e
pure v = C ($ v)

||| A command that produces no event.
|||
||| This will never fire an event.
export %inline
noAction : Cmd e
noAction = neutral

||| Use the given command conditionally.
|||
||| If the boolean flag is `False`, this will return the
||| empty command (`noAction`).
export
updateIf : Bool -> Lazy (Cmd e) -> Cmd e
updateIf True  u = u
updateIf False _ = noAction
