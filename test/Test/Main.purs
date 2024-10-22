module Test.Main
  ( Entry(..)
  , springConsts
  , testGraph
  )
  where

import Data.Graph
import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.List (scanl)
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec, oneX, getX)
import Effect (Effect)
import Effect.Class.Console (log)
import Springy (Particle, Point, SpringConsts, smul, updateNetwork)

springConsts :: SpringConsts
springConsts = {k: 1.0, dx: 1.0}
main :: Effect Unit
main = do
  log "🍕"
  log $ show $ (getVerts graph :: L.List (Tuple Int Particle))
  log $ show $ neighbors 0 graph
  log $ show $ map (\g -> map (\(Tuple _ p) -> getX p.x) (getVerts g :: L.List (Tuple Int Particle))) newGraphs
  where
    positionGraph = testGraph
    graph = map (\x -> {x: x, v: zero, m: 1.0}) positionGraph
    newGraphs = scanl go graph (L.range 1 20)
    go g _ = updateNetwork springConsts 0.1 g

data Entry k v = Entry k v (Array k)
testGraph :: Graph Int Point
testGraph = newGraph (
  M.fromFoldable [
    Tuple 1 (-1.0 `smul` oneX),
    Tuple 0 oneX
    ]
  ) (
    S.fromFoldable [Edge 0 1]
    )
