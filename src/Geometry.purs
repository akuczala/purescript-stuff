module Geometry
  ( Field
  , Point
  , circlePoints
  , closestPoint
  , distance
  , dot
  , smul
  ) where

import Prelude

import Data.Array ((..))
import Data.Foldable (minimumBy, sum)
import Data.Int (toNumber)
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe)
import Data.Number (cos, pi, sin, sqrt)
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..))

type Field = Number
type Point = Vec Field

smul :: Field -> Vec Field -> Vec Field
smul a v = map ((*) a) v

dot :: Vec Field -> Vec Field -> Field
dot v1 v2 = sum $ v1 * v2

distance :: Point -> Point -> Field
distance p1 p2 = let delta = p1 - p2 in sqrt $ dot delta delta

closestPoint :: forall k. Ord k => Map k Point -> Point -> Maybe (Tuple k Point)
closestPoint m p = minimumBy distanceOrder (toUnfoldable m :: Array _)
  where
  distanceOrder (Tuple _ p1) (Tuple _ p2) = compare (distance p1 p) (distance p2 p)

circlePoints :: { n :: Int, angle :: Number } -> Array Point
circlePoints r = do
  i <- 0 .. r.n
  let a = 2.0 * pi * (toNumber i) / (toNumber r.n) + r.angle
  pure $ Vec (cos a) (sin a)