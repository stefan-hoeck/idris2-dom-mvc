module Examples.Fractals

import Data.Either
import Data.Refined.Integer

import Derive.Prelude
import Derive.Refined

import Examples.CSS.Fractals
import Examples.Fractals.Dragon
import Examples.Util
import Web.MVC

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Model
--------------------------------------------------------------------------------

data Fractal = Dragon

MinIter, MaxIter, MinDelay, MaxDelay : Integer
MinIter  = 2
MaxIter  = 18
MinDelay = 100
MaxDelay = 10000

record Iterations where
  constructor I
  value : Integer
  0 prf : FromTo MinIter MaxIter value

namespace Iterations
  %runElab derive "Iterations" [Show,Eq,Ord,RefinedInteger]

record RedrawDelay where
  constructor D
  value : Integer
  0 prf : FromTo MinDelay MaxDelay value

namespace RedrawDelay
  %runElab derive "RedrawDelay" [Show,Eq,Ord,RefinedInteger]

readIter : String -> Either String Iterations
readIter =
  let err := "expected integer between \{show MinIter} and \{show MaxIter}"
   in maybeToEither err . refineIterations . cast

readDelay : String -> Either String RedrawDelay
readDelay =
  let err := "expected integer between \{show MinDelay} and \{show MaxDelay}"
   in maybeToEither err . refineRedrawDelay . cast

public export
data FractEv : Type where
  Init   : FractEv
  Fract  : Fractal -> FractEv
  Iter   : Either String Iterations -> FractEv
  Redraw : Either String RedrawDelay -> FractEv
  Run    : FractEv
  Inc    : FractEv

public export
record FractST where
  constructor FS
  fractal    : Fractal
  iterations : Iterations
  redraw     : RedrawDelay

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

content : Node FractEv
content =
  div [ class fractalContent ]
    [ lbl "Number of iterations:" lblIter
    , input [ ref txtIter
            , onInput (Iter . readIter)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [\{show MinIter}, \{show MaxIter}]"
            ] []
    , lbl "Iteration delay [ms]:" lblDelay
    , input [ ref txtRedraw
            , onInput (Redraw . readDelay)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [\{show MinDelay}, \{show MinDelay}]"
            ] []
    , button [ref btnRun, onClick Run, classes [widget,btn]] ["Run"]
    , div [ref out] []
    ]

-- --------------------------------------------------------------------------------
-- --          Controller
-- --------------------------------------------------------------------------------
--
-- -- msf : (timer : RedrawDelay -> JSIO ()) -> MSF JSIO Ev ()
-- -- msf timer = drswitchWhen neutral config fractal
-- --   where fractal : Config -> MSF JSIO Ev ()
-- --         fractal c =
-- --           let Element dragons prf = mkDragons c.iterations.value
-- --            in ifIs Inc $ cycle dragons >>> innerHtml out
-- --
-- --         readAll : MSF JSIO Ev (Either String Config)
-- --         readAll =    MkConfig Dragon
-- --                 <$$> getInput Iter   read txtIter
-- --                 <**> getInput Redraw read txtRedraw
-- --                 >>>  observeWith (isLeft ^>> disabledAt btnRun)
-- --
-- --         config : MSF JSIO Ev (MSFEvent Config)
-- --         config =   fan [readAll, is Run]
-- --                >>> rightOnEvent
-- --                >>> observeWith (ifEvent $ arrM (liftJSIO . timer . redraw))

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
--       timer   : RedrawDelay -> JSIO ()
--       timer ra = do
--         cleanup
--         newID <- setInterval ra.value (handle Inc)
--         writeIORef ref (Just newID)
--
--   pure (msf timer, cleanup)
