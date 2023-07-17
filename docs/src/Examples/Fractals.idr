module Examples.Fractals

import Data.DPair
import Data.Either
import Data.IORef
import Data.Nat
import Data.So
import Examples.CSS.Fractals
import Examples.Fractals.Dragon
import Examples.Util
import Derive.Prelude
import Web.MVC

%language ElabReflection
%default total

--------------------------------------------------------------------------------
--          Model
--------------------------------------------------------------------------------

data Fractal = Dragon

%runElab derive "Fractal" [Show,Eq]

data Ev = Fract Fractal | Iter | Redraw | Run | Inc

%runElab derive "Ev" [Show,Eq]

MaxIter : Nat
MaxIter = 18

record Iterations where
  constructor MkIterations
  value : Nat
  0 prf : LTE value MaxIter

namespace Iterations
  export
  fromInteger :  (n : Integer)
              -> {auto 0 prf : LTE (fromInteger n) MaxIter}
              -> Iterations
  fromInteger n = MkIterations (cast n) prf

  export
  read : String -> Either String Iterations
  read "0" = Right $ MkIterations 0 LTEZero
  read s = case cast {to = Nat} s of
    0 => Left "Not a natural number: \{s}"
    n => case isLTE n MaxIter of
      Yes prf   => Right $ MkIterations n prf
      No contra => Left "Value must be <= \{show MaxIter}"

isDelay : Bits32 -> Bool
isDelay v = 100 <= v && v <= 10000

record RedrawAfter where
  constructor RA
  value : Bits32
  0 prf : So (isDelay value)

namespace RedrawAfter
  export
  read : String -> Either String RedrawAfter
  read s =
    let v = cast {to = Bits32} s
     in case choose (isDelay v) of
          Left oh => Right $ RA v oh
          Right _ => Left $ "Enter a value between 100 and 10'000"

record Config where
  constructor MkConfig
  fractal    : Fractal
  iterations : Iterations
  redraw     : RedrawAfter

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

-- msf : (timer : RedrawAfter -> JSIO ()) -> MSF JSIO Ev ()
-- msf timer = drswitchWhen neutral config fractal
--   where fractal : Config -> MSF JSIO Ev ()
--         fractal c =
--           let Element dragons prf = mkDragons c.iterations.value
--            in ifIs Inc $ cycle dragons >>> innerHtml out
--
--         readAll : MSF JSIO Ev (Either String Config)
--         readAll =    MkConfig Dragon
--                 <$$> getInput Iter   read txtIter
--                 <**> getInput Redraw read txtRedraw
--                 >>>  observeWith (isLeft ^>> disabledAt btnRun)
--
--         config : MSF JSIO Ev (MSFEvent Config)
--         config =   fan [readAll, is Run]
--                >>> rightOnEvent
--                >>> observeWith (ifEvent $ arrM (liftJSIO . timer . redraw))

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

content : Node Ev
content =
  div [ class fractalContent ]
    [ lbl "Number of iterations:" lblIter
    , input [ ref txtIter
            , onInput (const Iter)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [0, \{show MaxIter}]"
            ] []
    , lbl "Iteration delay [ms]:" lblDelay
    , input [ ref txtRedraw
            , onInput (const Redraw)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [100,10'000]"
            ] []
    , button [ref btnRun, onClick Run, classes [widget,btn]] ["Run"]
    , div [ref out] []
    ]

--------------------------------------------------------------------------------
--          UI
--------------------------------------------------------------------------------

-- export
-- ui : Handler JSIO Ev => JSIO (MSF JSIO Ev (), JSIO ())
-- ui = do
--   innerHtmlAt exampleDiv content
--   ref  <- newIORef {a = Maybe IntervalID} Nothing
--
--   let cleanup : JSIO ()
--       cleanup = readIORef ref >>= traverse_ clearInterval
--
--       timer   : RedrawAfter -> JSIO ()
--       timer ra = do
--         cleanup
--         newID <- setInterval ra.value (handle Inc)
--         writeIORef ref (Just newID)
--
--   pure (msf timer, cleanup)
