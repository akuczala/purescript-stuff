module Springy
  ( Force
  , Particle
  , SpringConsts
  , Velocity
  , updateNetwork
  ) where

import Prelude

import Control.Monad.Reader (Reader, asks)
import Data.Foldable (class Foldable, sum)
import Data.Graph as G
import Data.List as L
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Vector2 (Vec)
import Geometry (Field, Point, dot, smul)

type Velocity = Vec Field
type Force = Vec Field

type Particle = { m :: Field, x :: Point, v :: Velocity }
type SpringConsts = { k :: Field, dx :: Field, drag :: Field }

pairForce :: Point -> Point -> Reader SpringConsts Force
pairForce p1 p2 = do
  k <- asks _.k
  dx <- asks _.dx
  pure $
    ( k * (dot delta delta - dx * dx)
    ) `smul` delta
  where
  delta = p1 - p2

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

netForce
  :: forall f
   . Foldable f
  => Traversable f
  => Functor f
  => f Particle
  -> Particle
  -> Reader SpringConsts Force
netForce parts part = do
  df <- dragForce part.v
  sf <- sum <$> traverse (\parti -> pairForce parti.x part.x) parts
  pure $ df + sf

-- TODO: put dt in reader
updateNetwork
  :: forall k
   . Ord k
  => Field
  -> G.Graph k Particle
  -> Reader SpringConsts (G.Graph k Particle)
updateNetwork dt g = traverseWithIndex stuff g
  where
  stuff label part = do
    f <- netForce (partNeighbors label) part
    pure $ update dt f part
  partNeighbors label = L.mapMaybe
    ( \neighborLabel -> G.lookup neighborLabel g
    )
    ( L.fromFoldable $ G.neighbors label g
    )

