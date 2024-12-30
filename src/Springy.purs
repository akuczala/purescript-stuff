module Springy
  ( Force
  , Particle
  , Velocity
  , firstOrderUpdate
  , netForce
  , updateNetwork
  , updateParticle
  ) where

import Prelude

import Constants (SpringConsts)
import Control.Monad.Reader (Reader, asks)
import Data.Foldable (class Foldable, sum)
import Data.Graph as G
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Traversable (class Traversable, sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), snd)
import Data.Vector2 (Vec)
import Geometry (Field, Point, distance, dot, smul)

type Velocity = Vec Field
type Force = Vec Field

type Particle = { m :: Field, x :: Point, v :: Velocity }

pairForce :: Point -> Point -> Reader SpringConsts Force
pairForce p1 p2 = do
  k <- asks _.k
  dx <- asks _.dx
  pure $
    ( k * (dot delta delta - dx * dx)
    ) `smul` delta
  where
  delta = p1 - p2

electricalForce :: Point -> Point -> Reader SpringConsts Force
electricalForce p1 p2 = do
  q <- asks _.q
  minElectricalDistance <- asks _.minElectricalDistance
  let regularizedDistance = max minElectricalDistance (distance p1 p2)
  pure $ (q / regularizedDistance) `smul` (p2 - p1)

dragForce :: Velocity -> Reader SpringConsts Force
dragForce v = do
  drag_coef <- asks _.drag
  pure $ -drag_coef `smul` v

update :: Field -> Force -> Particle -> Particle
update dt f part =
  let
    a = part.m `smul` f
    v = part.v + dt `smul` a
  in
    part { x = part.x + dt `smul` v, v = v }

firstOrderUpdate :: Field -> Force -> Particle -> Particle
firstOrderUpdate dt f part =
  let
    v = (dt * 2.0 * part.m) `smul` f
  in
    part { x = part.x + dt `smul` v, v = v }

netForce
  :: forall f g
   . Foldable f
  => Traversable f
  => Foldable g
  => Traversable g
  => { neighbors :: f Particle, allOthers :: g Particle }
  -> Particle
  -> Reader SpringConsts Force
netForce parts part = sum <$> sequence
  [ dragForce part.v
  , sum <$> traverse (\parti -> pairForce parti.x part.x) parts.neighbors
  , sum <$> traverse (\parti -> electricalForce parti.x part.x) parts.allOthers
  ]

-- updateSystem ::
--   forall f k
--   . Foldable f
--   => Traversable f
--   => Ord k
--   => Field
--   -> (k -> f Particle) -- neighbor function
--   -> (k -> Particle)
--   -> Reader SpringConsts (k -> Particle)
-- updateSystem dt neighborsOf g = traverseWithIndex stuff g
--   where
--   stuff label part = do
--     force <- netForce { neighbors: neighborsOf label, allOthers: allOthers label } part
--     pure $ firstOrderUpdate dt force part
--   allOthers :: k -> L.List Particle
--   allOthers label = g # M.toUnfoldable
--     >>> L.filter (\(Tuple k _) -> k /= label)
--     >>> map snd

updateParticle
  :: forall k
   . Ord k
  => Field
  -> G.Graph k Particle
  -> k
  -> Particle
  -> Reader SpringConsts Particle
updateParticle dt g label particle = do
  f <- netForce { neighbors, allOthers } particle
  pure $ firstOrderUpdate dt f particle
  where
  neighbors = L.mapMaybe
    ( \neighborLabel -> G.lookup neighborLabel g
    )
    ( L.fromFoldable $ G.neighbors label g
    )
  allOthers = g # G.getVerts
    >>> L.filter (\(Tuple k _) -> k /= label)
    >>> map snd

-- TODO: put dt in reader
updateNetwork
  :: forall k
   . Ord k
  => Field
  -> G.Graph k Particle
  -> Reader SpringConsts (G.Graph k Particle)
updateNetwork dt g = traverseWithIndex (updateParticle dt g) g
