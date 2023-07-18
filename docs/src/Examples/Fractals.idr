module Examples.Fractals

import Data.Either
import Data.Refined.Integer

import Derive.Prelude
import Derive.Refined

import Examples.CSS.Fractals
import Examples.Fractals.Dragon
import Examples.Util

import Web.MVC
import Web.MVC.Animate

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Model
--------------------------------------------------------------------------------

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
  Iter   : Either String Iterations -> FractEv
  Redraw : Either String RedrawDelay -> FractEv
  Run    : FractEv
  Inc    : DTime -> FractEv

public export
record FractST where
  constructor FS
  dragons  : List String
  itersIn  : Maybe Iterations
  redrawIn : Maybe RedrawDelay
  redraw   : RedrawDelay
  dtime    : DTime
  cleanUp  : IO ()

export
init : FractST
init = FS [] Nothing Nothing 500 0 (pure ())

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

content : Node FractEv
content =
  div [ class fractalContent ]
    [ lbl "Number of iterations:" lblIter
    , input [ Id txtIter
            , onInput (Iter . readIter)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [\{show MinIter}, \{show MaxIter}]"
            ] []
    , lbl "Iteration delay [ms]:" lblDelay
    , input [ Id txtRedraw
            , onInput (Redraw . readDelay)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [\{show MinDelay}, \{show MaxDelay}]"
            ] []
    , button [Id btnRun, onClick Run, classes [widget,btn]] ["Run"]
    , div [Id out] []
    ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

rotate : List t -> List t
rotate []     = []
rotate (h::t) = t ++ [h]

adjST : FractEv -> FractST -> FractST
adjST Init       s = init
adjST (Iter x)   s = {itersIn  := eitherToMaybe x} s
adjST (Redraw x) s = {redrawIn := eitherToMaybe x} s
adjST (Inc dt)   s =
  let dt2 := s.dtime + dt
   in if cast dt2 >= s.redraw.value
         then {dtime := 0, dragons $= rotate} s
         else {dtime := dt2} s
adjST Run        s =
  fromMaybe s $ do
    i <- s.itersIn
    r <- s.redrawIn
    pure $
      { redraw  := r
      , dtime   := 0
      , dragons := fst (mkDragons $ cast i.value)
      } s

dragonStr : List String -> String
dragonStr (h::t) = h
dragonStr []     = ""

displayST : FractST -> List (DOMUpdate FractEv)
displayST s =
  [ disabled btnRun $ null s.itersIn || null s.redrawIn
  , updateIf (s.dtime == 0) (child out . Raw $ dragonStr s.dragons)
  ]

displayEv : FractEv -> DOMUpdate FractEv
displayEv Init       = child exampleDiv content
displayEv (Iter x)   = validate txtIter x
displayEv (Redraw x) = validate txtRedraw x
displayEv Run        = NoAction
displayEv (Inc m)    = NoAction

display : FractEv -> FractST -> List (DOMUpdate FractEv)
display e s = displayEv e :: displayST s

export
runFract : Has FractEv es => SHandler es -> FractEv -> FractST -> JSIO FractST
runFract h Init s = do
  s2   <- injectDOM adjST display h Init s
  stop <- animate (h . inject . Inc)
  pure $ {cleanUp := stop} s2
runFract h e s = injectDOM adjST display h e s
