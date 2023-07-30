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
  {auto 0 prf : FromTo MinIter MaxIter value}

namespace Iterations
  %runElab derive "Iterations" [Show,Eq,Ord,RefinedInteger]

record RedrawDelay where
  constructor D
  value : Integer
  {auto 0 prf : FromTo MinDelay MaxDelay value}

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
  FractInit  : FractEv
  Iter       : Either String Iterations -> FractEv
  Redraw     : Either String RedrawDelay -> FractEv
  Run        : FractEv
  Inc        : DTime -> FractEv
  GotCleanup : IO () -> FractEv

public export
record FractST where
  constructor FS
  dragons  : List String
  itersIn  : Maybe Iterations
  redrawIn : Maybe RedrawDelay
  redraw   : RedrawDelay
  dtime    : DTime
  cleanup  : IO ()

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

export
update : FractEv -> FractST -> FractST
update FractInit       s = init
update (GotCleanup cu) s = {cleanup := cu} s
update (Iter x)   s = {itersIn  := eitherToMaybe x} s
update (Redraw x) s = {redrawIn := eitherToMaybe x} s
update (Inc dt)   s =
  let dt2 := s.dtime + dt
   in if cast dt2 >= s.redraw.value
         then {dtime := 0, dragons $= rotate} s
         else {dtime := dt2} s
update Run        s =
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

displayST : FractST -> Cmd FractEv
displayST s =
  batch
    [ disabled btnRun $ null s.itersIn || null s.redrawIn
    , updateIf (s.dtime == 0) (child out . Raw $ dragonStr s.dragons)
    ]

displayEv : FractEv -> Cmd FractEv
displayEv FractInit      =
  child exampleDiv content <+> animateWithCleanup GotCleanup Inc
displayEv (Iter x)       = validate txtIter x
displayEv (Redraw x)     = validate txtRedraw x
displayEv Run            = noAction
displayEv (GotCleanup _) = noAction
displayEv (Inc m)        = noAction

export
display : FractEv -> FractST -> Cmd FractEv
display e s = displayEv e <+> displayST s
