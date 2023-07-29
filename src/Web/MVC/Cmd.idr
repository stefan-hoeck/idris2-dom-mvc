module Web.MVC.Cmd

import Control.Monad.Either.Extra
import JS

%default total

--------------------------------------------------------------------------------
--          Commands
--------------------------------------------------------------------------------

||| A `Cmd` (abbreviation of "command") is a (typically effectful) computation
||| sending its result to an event handler.
public export
record Cmd (e : Type) where
  constructor C
  run : (e -> JSIO ()) -> JSIO ()

export %inline
Functor Cmd where
  map f (C run) = C $ run . (. f)

bind : {0 a,b : _} -> Cmd a -> (a -> Cmd b) -> Cmd b
bind (C g) f = C $ \h => g $ \va => run (f va) h

export %inline
Applicative Cmd where
  pure v = C $ \h => h v
  cf <*> cv = bind cf (<$> cv)

export %inline
Monad Cmd where
  (>>=) = bind

export
Semigroup (Cmd e) where
  C f <+> C g = C $ \h => f h >> g h

export %inline
Monoid (Cmd e) where
  neutral = C . const $ pure ()

||| Wrap a batch of commands in a single command by
||| running them sequentially.
export
batch : List (Cmd e) -> Cmd e
batch cs = C $ \h => traverseList_ (`run` h) cs

||| Wrap an effectful computation in a command.
export %inline
cmd : JSIO e -> Cmd e
cmd v = C (v >>=)

||| Wrap an effectful computation in a command.
export %inline
cmd_ : JSIO () -> Cmd e
cmd_ v = C $ const v

||| A command that produces no event.
export %inline
noAction : Cmd e
noAction = neutral

export
updateIf : Bool -> Lazy (Cmd e) -> Cmd e
updateIf True  u = u
updateIf False _ = noAction

export %inline
HasIO Cmd where
  liftIO = cmd . liftIO
