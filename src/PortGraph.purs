module PortGraph
  ( PortConnection(..)
  , PortGraph
  , _nodeLabel
  , _port
  , getNodePorts
  , nodeOrder
  , toGraph
  ) where

import Prelude

import Data.Graph (_edges)
import Data.Graph as G
import Data.Lens (Lens, lens, over, view)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))

data PortConnection k p = PortConnection k p

_port :: forall k pa pb. Lens (PortConnection k pa) (PortConnection k pb) pa pb
_port = lens (\(PortConnection _ p) -> p) (\(PortConnection k _) pb -> PortConnection k pb)

_nodeLabel :: forall ka kb p. Lens (PortConnection ka p) (PortConnection kb p) ka kb
_nodeLabel = lens (\(PortConnection k _) -> k) (\(PortConnection _ p) kb -> PortConnection kb p)

derive instance (Eq k, Eq p) => Eq (PortConnection k p)
derive instance (Ord k, Ord p) => Ord (PortConnection k p)

type PortGraph k p v = G.GraphF k (PortConnection k p) v

toGraph :: forall p k v. Ord k => PortGraph k p v -> G.Graph k v
toGraph = over G._edges f
  where
  f = S.map (\(G.Edge pc1 pc2) -> G.Edge (view _nodeLabel pc1) (view _nodeLabel pc2))

-- returns a map of a node's ports to the label of the destination node
getNodePorts :: forall k p v. Ord p => Ord k => k -> PortGraph k p v -> M.Map p k
getNodePorts k g = view _edges g # S.mapMaybe f # M.fromFoldable
  where
  f (G.Edge (PortConnection k1 p1) (PortConnection k2 p2))
    | k1 == k = Just (Tuple p1 k2)
    | k2 == k = Just (Tuple p2 k1)
  f _ = Nothing

nodeOrder :: forall k p v. Ord k => Ord p => k -> PortGraph k p v -> Int
nodeOrder nodeLabel g = M.size $ getNodePorts nodeLabel g