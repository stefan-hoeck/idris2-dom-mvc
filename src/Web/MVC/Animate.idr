||| Utilities not (yet) available from idris2-dom
module Web.MVC.Animate

import Data.IORef
import JS
import Web.MVC.Cmd

--------------------------------------------------------------------------------
--          Time
--------------------------------------------------------------------------------

%foreign "javascript:lambda:(w) => BigInt(new Date().getTime())"
prim__time : PrimIO Integer

||| Get the current time in milliseconds since 1970/01/01.
export
currentTime : HasIO io => io Integer
currentTime = primIO prim__time

||| Determine the time taken to run a command and wrap it in an
||| event.
export
timed : (Integer -> e) -> Cmd e -> Cmd e
timed toEv (C f) = C $ \h => do
  t1 <- currentTime
  f h
  t2 <- currentTime
  h (toEv $ t2 - t1)

--------------------------------------------------------------------------------
--          Timers
--------------------------------------------------------------------------------

||| ID used to identify and cancel a running timer.
public export
data IntervalID : Type where [external]

%foreign "browser:lambda:(n,h,w)=>setInterval(() => h(w),n)"
prim__setInterval : Bits32 -> IO () -> PrimIO IntervalID

%foreign "browser:lambda:(i,w)=>clearInterval(i)"
prim__clearInterval : IntervalID -> PrimIO ()

||| Sets a timer to repeatedly carry out the given IO action
||| after the given number of milliseconds.
|||
||| Returns an ID, which can be used with `clearInterval` to
||| cancel the timer.
export
every : e -> Bits32 -> Cmd e
every ev millis =
  C $ \h => ignore $ primIO (prim__setInterval millis (runJS $ h ev))

||| Cancel a running timer with the given ID.
export
everyWithCleanup : (IntervalID -> e) -> e -> Bits32 -> Cmd e
everyWithCleanup idToEv ev millis =
  C $ \h => Prelude.do
    id <- primIO (prim__setInterval millis (runJS $ h ev))
    h (idToEv id)

--------------------------------------------------------------------------------
--          Animations
--------------------------------------------------------------------------------

%foreign """
         browser:lambda:(stop,h,w)=>{
            let previousTimeStamp;

            function step(timestamp) {
              if (previousTimeStamp === undefined)
                previousTimeStamp = timestamp;
              const dtime = timestamp - previousTimeStamp;
              previousTimeStamp = timestamp;
              if (stop(w) === 0) {
                h(dtime)(w)
                window.requestAnimationFrame(step);
              }
            }

            window.requestAnimationFrame(step);
         }
         """
prim__animate : IO Bits32 -> (Bits32 -> IO ()) -> PrimIO ()

||| Alias for a time delta in milliseconds
public export
DTime : Type
DTime = Bits32

||| Use `window.requestAnimationFrame` to repeatedly
||| animate the given function.
|||
||| The function takes the time delta (in milliseconds) since
||| the previous animation step as input.
|||
||| Returns a cleanup action, which can be run to
||| stop the running animation.
export
animate : (DTime -> e) -> Cmd e
animate toEv = C $ \h => Prelude.do
  primIO $ prim__animate (pure 0) (runJS . h . toEv)

||| Use `window.requestAnimationFrame` to repeatedly
||| animate the given function.
|||
||| The function takes the time delta (in milliseconds) since
||| the previous animation step as input.
|||
||| Returns a cleanup action, which can be run to
||| stop the running animation.
export
animateWithCleanup : (IO () -> e) -> (DTime -> e) -> Cmd e
animateWithCleanup cleanupToEv toEv = C $ \h => Prelude.do
  ref <- newIORef (the Bits32 0)
  primIO $ prim__animate (readIORef ref) (runJS . h . toEv)
  h $ cleanupToEv (writeIORef ref 1)
