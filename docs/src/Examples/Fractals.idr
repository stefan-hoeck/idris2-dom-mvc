module Examples.Fractals

import Data.Either
import Data.Refined.Bits32
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

MinIter, MaxIter : Integer
MinIter  = 2
MaxIter  = 18

MinDelay, MaxDelay : Bits32
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
  value : Bits32
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
  Inc        : FractEv
  GotCleanup : IO () -> FractEv

public export
record FractST where
  constructor FS
  dragons  : List String
  itersIn  : Maybe Iterations
  redrawIn : Maybe RedrawDelay
  cleanup  : IO ()

(.input) : FractST -> Maybe (Iterations,RedrawDelay)
s.input = [| MkPair s.itersIn s.redrawIn |]

export
init : FractST
init = FS [] Nothing Nothing (pure ())

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

content : Node FractEv
content =
  div [ class fractalContent ]
    [ lbl "Number of iterations:" lblIter
    , input
        [ Id txtIter
        , onInput (Iter . readIter)
        , onEnterDown Run
        , class widget
        , placeholder "Range: [\{show MinIter}, \{show MaxIter}]"
        ] []
    , lbl "Iteration delay [ms]:" lblDelay
    , input
        [ Id txtRedraw
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
update  Inc       s = {dragons  $= rotate} s
update Run        s = case s.input of
  Nothing    => s
  Just (i,_) => {dragons := fst (mkDragons $ cast i.value)} s

dragonStr : List String -> String
dragonStr (h::t) = h
dragonStr []     = ""

export
display : FractEv -> FractST -> Cmd FractEv
display FractInit      _ = child exampleDiv content
display (Iter x)       s = validate txtIter x <+> disabledM btnRun s.input
display (Redraw x)     s = validate txtRedraw x <+> disabledM btnRun s.input
display (GotCleanup _) _ = noAction
display  Inc           s = child out $ Raw (dragonStr s.dragons)
display Run            s =
  cmdIfJust s.input $ \(i,r) =>
    liftIO_ s.cleanup <+> everyWithCleanup GotCleanup Inc r.value
