module Data.Graph
  ( Edge(..)
  , Graph
  , edgeContains
  , getEdges
  , getVerts
  , lookup
  , modifyVertex
  , neighbors
  , newGraph
  , setVertex
  , toMap
  ) where

import Prelude

import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

data Edge k = Edge k k

edgeContains :: forall k. Ord k => k -> Edge k -> Boolean
edgeContains k (Edge k1 k2) = case [ compare k k1, compare k k2 ] of
  [ EQ, EQ ] -> true
  _ -> false

edgeOther :: forall k. Ord k => k -> Edge k -> Maybe k
edgeOther k (Edge k1 k2)
  | k == k1 = Just k2
  | k == k2 = Just k1
  | otherwise = Nothing

newtype Graph k v = Graph { verts :: (M.Map k v), edges :: (S.Set (Edge k)) }

newGraph :: forall k v. (M.Map k v) -> (S.Set (Edge k)) -> Graph k v
newGraph vs es = Graph { verts: vs, edges: es }

neighbors :: forall k v. Ord k => k -> Graph k v -> S.Set k
neighbors k (Graph g) = S.mapMaybe (edgeOther k) g.edges

setVertex :: forall k v. Ord k => k -> v -> Graph k v -> Graph k v
setVertex k v (Graph g) = newGraph (M.insert k v g.verts) g.edges

modifyVertex :: forall k v. Ord k => k -> (v -> v) -> Graph k v -> Graph k v
modifyVertex k f g = case lookup k g of
  Just v -> setVertex k (f v) g
  Nothing -> g

vertexMap :: forall k a b. (a -> b) -> Graph k a -> Graph k b
vertexMap f (Graph g) = newGraph (map f g.verts) g.edges

toMap :: forall k v. Graph k v -> M.Map k v
toMap (Graph g) = g.verts

getVerts :: forall f k v. Unfoldable f => Graph k v -> f (Tuple k v)
getVerts (Graph g) = M.toUnfoldable g.verts

getEdges :: forall k v. Graph k v -> S.Set (Edge k)
getEdges (Graph g) = g.edges

lookup :: forall k v. Ord k => k -> Graph k v -> Maybe v
lookup k (Graph g) = M.lookup k g.verts

instance Eq a => Eq (Edge a) where
  eq (Edge a b) (Edge c d) = eq (Tuple a b) (Tuple c d)

instance Ord a => Ord (Edge a) where
  compare (Edge a b) (Edge c d) = compare (Tuple a b) (Tuple c d)

instance Functor (Graph k) where
  map = vertexMap

instance FunctorWithIndex k (Graph k) where
  mapWithIndex f (Graph g) = newGraph (mapWithIndex f g.verts) g.edges