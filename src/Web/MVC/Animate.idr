||| Utilities not (yet) available from idris2-dom
module Web.MVC.Animate

import Data.IORef
import JS

--------------------------------------------------------------------------------
--          Time
--------------------------------------------------------------------------------

%foreign "javascript:lambda:(w) => BigInt(new Date().getTime())"
prim__time : PrimIO Integer

||| Get the current time in milliseconds since 1970/01/01.
export
currentTime : HasIO io => io Integer
currentTime = primIO prim__time

||| Determine the time taken to run the given action and return
||| the difference together with the action's result.
export
timed : JSIO a -> JSIO (a,Integer)
timed act = do
  t1 <- currentTime
  v  <- act
  t2 <- currentTime
  pure (v, t2 - t1)

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
setInterval : HasIO io => Bits32 -> JSIO () -> io (IntervalID)
setInterval millis run = primIO $ prim__setInterval millis (runJS run)

||| Cancel a running timer with the given ID.
export
clearInterval : HasIO io => IntervalID -> io ()
clearInterval id = primIO $ prim__clearInterval id

--------------------------------------------------------------------------------
--          Animations
--------------------------------------------------------------------------------

%foreign """
         browser:lambda:(h,w)=>{
            let previousTimeStamp;
            let stop = 0;

            function step(timestamp) {
              if (previousTimeStamp === undefined)
                previousTimeStamp = timestamp;
              const dtime = timestamp - previousTimeStamp;
              previousTimeStamp = timestamp;
              stop = h(dtime)(w);
              if (stop === 0) {
                window.requestAnimationFrame(step);
              }
            }

            window.requestAnimationFrame(step);
         }
         """
prim__animate : (Bits32 -> IO Bits32) -> PrimIO ()

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
animate : HasIO io => (DTime -> JSIO ()) -> io (IO ())
animate run = do
  ref <- newIORef (the Bits32 0)
  primIO $ prim__animate (\dt => runJS (run dt) >> readIORef ref)
  pure (writeIORef ref 1)
