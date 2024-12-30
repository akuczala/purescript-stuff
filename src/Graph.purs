module Data.Graph
  ( Edge(..)
  , Graph
  , GraphF(..)
  , _edges
  , _verts
  , addEdge
  , addVertex
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

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Lens (Lens, lens, over, view)
import Data.Map (findMax)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as S
import Data.Traversable (class Traversable, traverse, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

data Edge k = Edge k k

instance Functor Edge where
  map f (Edge k1 k2) = Edge (f k1) (f k2)

edgeContains :: forall k. Ord k => k -> Edge k -> Boolean
edgeContains k (Edge k1 k2) = case [ compare k k1, compare k k2 ] of
  [ EQ, EQ ] -> true
  _ -> false

edgeOther :: forall k. Ord k => k -> Edge k -> Maybe k
edgeOther k (Edge k1 k2)
  | k == k1 = Just k2
  | k == k2 = Just k1
  | otherwise = Nothing

newtype GraphF k p v = Graph { verts :: (M.Map k v), edges :: (S.Set (Edge p)) }

instance Newtype (GraphF k p v) { verts :: (M.Map k v), edges :: (S.Set (Edge p)) }

_verts
  :: forall k p a b
   . Lens (GraphF k p a) (GraphF k p b) (M.Map k a) (M.Map k b)
_verts = lens (unwrap >>> _.verts) (\g vs -> wrap $ (unwrap g) { verts = vs })

_edges
  :: forall k v a b
   . Lens (GraphF k a v) (GraphF k b v) (S.Set (Edge a)) (S.Set (Edge b))
_edges = lens (unwrap >>> _.edges) (\g es -> wrap $ (unwrap g) { edges = es })

type Graph k v = GraphF k k v

newGraph :: forall k p v. (M.Map k v) -> (S.Set (Edge p)) -> GraphF k p v
newGraph vs es = Graph { verts: vs, edges: es }

neighbors :: forall k v. Ord k => k -> Graph k v -> S.Set k
neighbors k (Graph g) = S.mapMaybe (edgeOther k) g.edges

setVertex :: forall k p v. Ord k => k -> v -> GraphF k p v -> GraphF k p v
setVertex k v = over _verts (M.insert k v)

addVertex :: forall v. v -> Graph Int v -> Graph Int v
addVertex v (Graph g) = setVertex newLabel v (Graph g)
  where
  newLabel = 1 + (fromMaybe (-1) <<< map _.key $ findMax g.verts)

modifyVertex
  :: forall k p v
   . Ord k
  => k
  -> (v -> v)
  -> GraphF k p v
  -> GraphF k p v
modifyVertex k f g = case lookup k g of
  Just v -> setVertex k (f v) g
  Nothing -> g

addEdge :: forall k p v. Ord p => Edge p -> GraphF k p v -> GraphF k p v
addEdge e = over _edges (S.insert e)

vertexMap :: forall k p a b. (a -> b) -> GraphF k p a -> GraphF k p b
vertexMap f g = over _verts (map f) g

toMap :: forall k p v. GraphF k p v -> M.Map k v
toMap = view _verts

getVerts :: forall f k v p. Unfoldable f => GraphF k p v -> f (Tuple k v)
getVerts (Graph g) = M.toUnfoldable g.verts

getEdges :: forall k p v. GraphF k p v -> S.Set (Edge p)
getEdges = view _edges

lookup :: forall k p v. Ord k => k -> GraphF k p v -> Maybe v
lookup k (Graph g) = M.lookup k g.verts

instance Eq a => Eq (Edge a) where
  eq (Edge a b) (Edge c d) = eq (Tuple a b) (Tuple c d)

instance Ord a => Ord (Edge a) where
  compare (Edge a b) (Edge c d) = compare (Tuple a b) (Tuple c d)

instance Functor (GraphF k p) where
  map = vertexMap

instance FunctorWithIndex k (GraphF k p) where
  mapWithIndex f (Graph g) = newGraph (mapWithIndex f g.verts) g.edges

instance Foldable (GraphF k p) where
  foldl f b0 (Graph g) = foldl f b0 g.verts
  foldr f b0 (Graph g) = foldr f b0 g.verts
  foldMap f (Graph g) = foldMap f g.verts

instance FoldableWithIndex k (GraphF k p) where
  foldlWithIndex f b0 (Graph g) = (foldlWithIndex f b0 g.verts)
  foldrWithIndex f b0 (Graph g) = (foldrWithIndex f b0 g.verts)
  foldMapWithIndex f (Graph g) = (foldMapWithIndex f g.verts)

instance Traversable (GraphF k p) where
  traverse f (Graph g) = map (\verts -> newGraph verts g.edges) (traverse f g.verts)
  sequence (Graph g) = map (\verts -> newGraph verts g.edges) (sequence g.verts)

instance TraversableWithIndex k (GraphF k p) where
  traverseWithIndex f (Graph g) = map (\verts -> newGraph verts g.edges) (traverseWithIndex f g.verts)