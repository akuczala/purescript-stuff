module Geometry
  ( Field
  , Point
  , closestPoint
  , distance
  , dot
  , smul
  ) where

import Prelude

import Data.Foldable (minimumBy, sum)
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe)
import Data.Number (sqrt)
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec)

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

