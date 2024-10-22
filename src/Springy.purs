module Springy
  ( Field
  , Force
  , Particle
  , Point
  , SpringConsts
  , Velocity
  , netForce
  , smul
  , update
  , updateNetwork
  )
  where

import Prelude

import Data.Foldable (class Foldable, sum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Graph as G
import Data.List as L
import Data.Vector2 (Vec, vec)

type Field = Number
smul :: Field -> Vec Field -> Vec Field
smul a v = map ((*) a) v

type Point = Vec Field
type Velocity = Vec Field
type Force = Vec Field

type Particle = {m :: Field, x :: Point, v :: Velocity}
type SpringConsts = {k :: Field, dx :: Field}

pairForce :: SpringConsts -> Point -> Point -> Force
pairForce consts p1 p2 = consts.k `smul` (p1 - p2)

update :: Field -> Force -> Particle -> Particle
update dt f part = let
    a = part.m `smul` f
    v = dt `smul` a
    in part {x = part.x + dt `smul` v, v = v}

netForce :: forall f. Foldable f => Functor f => SpringConsts -> f Particle -> Particle -> Force
netForce consts parts part = sum $ map (\parti -> pairForce consts parti.x part.x) parts

updateNetwork :: forall k. Ord k => SpringConsts -> Field -> G.Graph k Particle -> G.Graph k Particle
updateNetwork consts dt g = mapWithIndex stuff g where
    stuff label part = update dt (force label part) part
    force label part = netForce consts (partNeighbors label) part
    partNeighbors label = L.mapMaybe (\neighborLabel -> G.lookup neighborLabel g) (L.fromFoldable $ G.neighbors label g)

