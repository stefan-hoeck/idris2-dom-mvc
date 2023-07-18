# Running Animations: Bouncing Balls

In this tutorial we are going to have a look at
running a (non-interactive) animation. We simulate
the frictionless movement of a group of balls under
the influence of gravitation in a two-dimensional
room.

The user interface will be very simple: Just a
validated text input for defining the number of
balls to animate and a button to (re)start the
animation. The main focus of the tutorial will
be the animation itself.

```idris
module Examples.Balls

import Data.Either
import Data.Nat
import Data.Vect

import Examples.CSS.Colors
import Examples.CSS.Balls
import Examples.Util

import Text.CSS.Color
import Web.MVC
import Web.MVC.Animate
import Web.MVC.Canvas

%default total
```

## Model

We first define a couple of physical entities:

```idris
-- 2D Vector
V2 : Type
V2 = Vect 2 Double

-- Velocity of a point in 2D space
Velocity : Type
Velocity = V2

-- Acceleration of a point in 2D space
Acceleration : Type
Acceleration = V2

-- constant acceleration vector
acc : Acceleration
acc = [0,-9.81]

-- height and width of the room in m
w : Double
w = 10

-- start height of all balls
h0 : Double
h0 = 9

-- ball radius in m
r : Double
r = 0.1

-- start velocity in m/s
v0 : Double
v0 = 4

(+) : V2 -> V2 -> V2
[u,v] + [x,y] = [u+x, v+y]

(*) : Double -> V2 -> V2
m * [x,y] = [m * x, m * y]

fpsCount : Nat
fpsCount = 15
```

We need a data type to hold the current state of a
ball in motion: Its color, position and velocity:

```idris
record Ball where
  constructor MkBall
  col : Color
  pos : V2
  vel : Velocity
```

## View

We draw our set of balls in a canvas, so we need
some instructions for doing so. A ball will sometimes
move beyond its physical boundaries, in which case the
controller (see below) will adjust its direction
of movement and it will move back into the room.
To get the illusion of reflecting the ball at the
correct location, we hide the ball as long as it is
outside the room (this happens only for very short
moments due to the limited time resolution of
our animation):

```idris
inBounds : Ball -> Bool
inBounds (MkBall _ [x,y] _) = y >= 0 && x >= 0 && x <= w

ballToScene : Ball -> Scene
ballToScene b@(MkBall _ [x,y] _) =
  S1 [Fill $ if inBounds b then b.col else transparent] Id $
    circle x (w - y) r Fill
```

The utilities for describing and rendering a canvas scene
can be found at `Rhone.Canvas` and its submodules.

We also draw some primitive walls and a floor to visualize
the room:

```idris
-- room wall thickness in meters
wallThickness : Double
wallThickness = 0.20

-- walls and floor of the room.
walls : Shape
walls =
  let hwt = wallThickness / 2
   in polyLine [(-hwt, 0), (-hwt, w+hwt), (w+hwt,w+hwt), (w+hwt,0)]
```

We can now describe a scene of balls plus the room
at a given point in time:

```idris
ballsToScene : List Ball -> Scene
ballsToScene bs =
  SM  [] (Transform 50 0 0 50 10 10) $
    [ SM [] Id $ map ballToScene bs
    , S1 [Stroke base80, LineWidth wallThickness] Id walls
    ]
```

Of course, we also need to set up the HTML objects of
our application plus the events they fire: `Next dt`
is used to move the animation forward by `dt`
milliseconds.

```idris
public export
data BallsEv : Type where
  BallsInit  : BallsEv
  Run        : BallsEv
  NumIn      : Either String Nat -> BallsEv
  Next       : DTime -> BallsEv

public export
record BallsST where
  constructor BS
  balls    : List Ball
  count    : Nat
  dtime    : DTime
  numBalls : Maybe Nat
  cleanUp  : IO ()

export
init : BallsST
init = BS [] fpsCount 0 Nothing (pure ())

-- canvas width and height
wcanvas : Bits32
wcanvas = 520

read : String -> Either String Nat
read s =
  let n = cast {to = Nat} s
   in if 0 < n && n <= 5000
        then Right n
        else Left "Enter a number between 1 and 1000"

content : Node BallsEv
content =
  div [ class ballsContent ]
    [ lbl "Number of balls:" lblCount
    , input [ Id txtCount
            , onInput (NumIn . read)
            , onEnterDown Run
            , class widget
            , placeholder "Range: [1,5000]"
            ] []
    , button [Id btnRun, onClick Run, classes [widget,btn]] ["Run"]
    , div [Id log] []
    , canvas [Id out, width wcanvas, height wcanvas] []
    ]
```

## Controller

The main focus of the controller will be to properly
animate the bouncing balls. We could try and use a functional
reactive programming approach implemented on top of
monadic stream functions. However, while MSFs perform
reasonably well, their monadic contexts will impose
some overhead, and we don't want that when animating
lots of objects (I tried this application first
with having a dedicated MSF for each individual ball:
Up to about 500 balls, performance was pretty good
at about 40 FPS on my machine, which is very
promising for programming interactive animations).

For calculating the next position and velocity vector
of a ball, we use simple Newtonian physics and some
help from the `VectorSpace` interface. We
also need some form of collision detection to make
sure our balls don't leave the room:

```idris
-- Collision detection: We verify that the given ball
-- is still in the room. If this is not the case, we simulate
-- a bouncing off the walls by inverting the x-velocity (if the
-- ball hit a wall) or the y-velocity (if the ball hit the ground)
checkBounds : Ball -> Ball
checkBounds b@(MkBall c [px,py] [vx,vy]) =
  if      (py <= r  && vy < 0)      then (MkBall c [px,py] [vx,-vy])
  else if (px <= r  && vx < 0)      then (MkBall c [px,py] [-vx,vy])
  else if (px >= (w - r) && vx > 0) then (MkBall c [px,py] [-vx,vy])
  else b

-- moves a ball after a given time delta
-- by adjusting its position and velocity
nextBall : DTime -> Ball -> Ball
nextBall delta (MkBall c p v) =
  let dt   = cast delta / the Double 1000 -- time in seconds
      v2   = v + (dt * acc)
      p2   = p + (dt / 2 * (v + v2))
   in checkBounds (MkBall c p2 v2)
```

We also need a way to create an initial set of
balls based on user input. We evenly distribute
them at a height of nine meters, giving them
slightly different colors and starting velocities:

```idris
||| Generates a list of balls to start the simulation.
export
initialBalls : (n : Nat) -> List Ball
initialBalls n = go n Nil
  where col : Bits8 -> Color
        col 0 = comp100
        col 1 = comp80
        col 2 = comp60
        col 3 = comp40
        col _ = comp20

        ball : Nat -> Ball
        ball k =
          let factor = cast {to = Double} k / (cast n - 1.0)
              phi    = pi * factor
              x0     = 1.0 + factor * 8
           in MkBall (col $ cast k `mod` 5) [x0,9] (v0 * [sin phi, cos phi])

        go : (k : Nat) -> List Ball -> List Ball
        go 0     bs = bs
        go (S k) bs = go k $ ball k :: bs
```

The controller handling the animation will use a state
accumulator for the balls and advance them the given
number of milliseconds:

```idris
export
showFPS : Bits32 -> String
showFPS 0 = ""
showFPS n =
  let val := 1000 * cast fpsCount `div` n
   in "FPS: \{show val}"

adjST : BallsEv -> BallsST -> BallsST
adjST BallsInit _ = init
adjST Run       s = {balls := maybe s.balls initialBalls s.numBalls} s
adjST (NumIn x) s = {numBalls := eitherToMaybe x} s
adjST (Next m)  s = case s.count of
  0   => { balls $= map (nextBall m), dtime := 0, count := fpsCount } s
  S k => { balls $= map (nextBall m), dtime $= (+m), count := k } s

displayST : BallsST -> List (DOMUpdate BallsEv)
displayST s =
  [ disabledM btnRun s.numBalls
  , render out (ballsToScene s.balls)
  , updateIf (s.count == 0) (text log $ showFPS s.dtime)
  ]

displayEv : BallsEv -> DOMUpdate BallsEv
displayEv BallsInit = child exampleDiv content
displayEv Run       = noAction
displayEv (NumIn x) = validate txtCount x
displayEv (Next m)  = noAction

display : BallsEv -> BallsST -> List (DOMUpdate BallsEv)
display e s = displayEv e :: displayST s

export
runBalls : Handler BallsEv => Controller BallsST BallsEv
runBalls BallsInit s = do
  s2   <- runDOM adjST display BallsInit s
  stop <- animate (handle . Next)
  pure $ {cleanUp := stop} s2
runBalls e s = runDOM adjST display e s
```

We now only need to write the controllers for reading user
input and running the application:

<!-- vi: filetype=idris2:syntax=markdown
-->
