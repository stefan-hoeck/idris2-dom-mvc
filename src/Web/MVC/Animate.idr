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

||| Determine the time taken to setup a command and wrap it in an
||| event that will be fired synchronously.
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

||| Fires the given event every `n` milliseconds.
|||
||| Note: Use `animate` for smoothly running animations.
export
every : e -> (n : Bits32) -> Cmd e
every ev millis =
  C $ \h => ignore $ primIO (prim__setInterval millis (runJS $ h ev))

||| Fires the given event every `n` milliseconds.
|||
||| In addition, this synchronously fires an event with a wrapped
||| handle for stopping the timer.
export
everyWithCleanup : (IO () -> e) -> e -> Bits32 -> Cmd e
everyWithCleanup cleanUpToEv ev millis =
  C $ \h => Prelude.do
    id <- primIO (prim__setInterval millis (runJS $ h ev))
    h (cleanUpToEv $ primIO (prim__clearInterval id))

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

||| Repeatedly fires the given event holding the time delta in
||| milliseconds since the last animation step.
export
animate : (DTime -> e) -> Cmd e
animate toEv = C $ \h => Prelude.do
  primIO $ prim__animate (pure 0) (runJS . h . toEv)

||| Repeatedly fires the given event holding the time delta in
||| milliseconds since the last animation step.
|||
||| In addition, synchronously fires an event with a wrapped
||| handle for stopping the animation.
export
animateWithCleanup : (IO () -> e) -> (DTime -> e) -> Cmd e
animateWithCleanup cleanupToEv toEv = C $ \h => Prelude.do
  ref <- newIORef (the Bits32 0)
  primIO $ prim__animate (readIORef ref) (runJS . h . toEv)
  h $ cleanupToEv (writeIORef ref 1)
