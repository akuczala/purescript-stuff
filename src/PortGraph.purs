module PortGraph
  ( Port(..)
  , toGraph
  ) where

import Prelude

import Data.Graph as G
import Data.Lens (over)
import Data.Set as S

data Port k p = Port k p

derive instance (Eq k, Eq p) => Eq (Port k p)

type PortGraph k p v = G.GraphF k (Port k p) v

toGraph :: forall p k v. Ord k => PortGraph k p v -> G.Graph k v
toGraph = over G._edges f
  where
  f = S.map (\(G.Edge (Port k1 _) (Port k2 _)) -> G.Edge k1 k2)