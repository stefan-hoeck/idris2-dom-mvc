||| Ugly code for calculating and rendering a dragon curve
||| in SVG. TODO: Cleanup, document.
module Examples.Fractals.Dragon

import Data.DPair
import Data.List
import Data.String

record Point where
  constructor P
  x,y : Int32

Num Point where
  fromInteger n = P (fromInteger n) 0
  P x1 y1 + P x2 y2 = P (x1 + x2) (y1 + y2)
  P x1 y1 * P x2 y2 = P (x1 * x2 - y1 * y2) (x1 * y2 + x2 * y1)

Neg Point where
  negate (P x y) = P (-x) (-y)
  P x1 y1 - P x2 y2 = P (x1 - x2) (y1 - y2)

rotate90 : Point -> Point
rotate90 (P x y) = P y (-x)

rotateAround90 : (origin : Point) -> Point -> Point
rotateAround90 o p = rotate90 (p - o) + o

Dragon : Type
Dragon = Subset (List Point) NonEmpty

0 lemma :  (as : List a)
        -> (v : a)
        -> (as2 : List a)
        -> NonEmpty (as ++ v :: as2)
lemma []       v as2 = IsNonEmpty
lemma (h :: t) v as2 = IsNonEmpty

firstDragon : Dragon
firstDragon = Element [0,800] IsNonEmpty

nextDragon : Dragon -> Dragon
nextDragon (Element (h :: t) prf) =
  let new = map (rotateAround90 h) (reverse t)
   in Element (new ++ h :: t) (lemma new h t)

dragonSVG : (n : Nat) -> Dragon -> String
dragonSVG n (Element ps _) =
  let fact = pow 2 (cast (n + 2) / 2.0)
      scale = 1.0 / fact

      attr = fastConcat $ map (\(P x y) => #"\#{show x}, \#{show y} "#) ps
      header =
        the String #"""
                   <svg version="1.1"
                        width="100%"
                        viewBox="0 0 1000 1000"
                        xmlns="http://www.w3.org/2000/svg">
                   """#
   in #"""
      \#{header}
        <polyline points="\#{attr}"
                  fill="none"
                  stroke="red"
                  vector-effect="non-scaling-stroke"
                  transform="translate (500,500) scale(\#{show scale})"
                  stroke-width="1"/>
      </svg>
      """#

export
mkDragons : (n : Nat) -> Subset (List String) NonEmpty
mkDragons n =
  let fd   = firstDragon
   in Element (dragonSVG 0 fd :: go fd n 1) IsNonEmpty
  where go : Dragon -> (rem : Nat) -> (iter : Nat) -> List String
        go _  0     _ = Nil
        go dr (S k) n =
          let nextDr = nextDragon dr
           in dragonSVG n nextDr :: go nextDr k (n+1)
