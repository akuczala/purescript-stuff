module Port.State
  ( GlobalState
  , GraphState
  , NodeParticle
  , NodePos
  , _graph
  , _particle
  , initialState
  , particleToPos
  , simpleGraph
  )
  where

import Prelude

import Data.Graph (Edge(..), newGraph)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..))
import Geometry (Point)
import Graphics.Canvas (Context2D)
import Port.InteractionNet (IPort(..), NodeType(..))
import PortGraph (PortConnection(..), PortGraph)
import Springy (Particle)
import Type.Proxy (Proxy(..))
import Web.DOM (Element)

type NodeParticle = {nodeType :: NodeType, particle :: Particle}

_particle :: forall a r. Lens' { particle :: a | r } a
_particle = prop (Proxy :: Proxy "particle")

type NodePos = {nodeType :: NodeType, pos :: Point}


particleToPos :: NodeParticle -> NodePos
particleToPos p = {nodeType: p.nodeType, pos: p.particle.x}

type GraphState = PortGraph Int IPort NodeParticle

type GlobalState =
  { graph :: GraphState
  , ctx :: Context2D
  , canvas :: Element
  , mousePos :: Maybe Point
  , mouseHeld :: Boolean
  , dragging :: Boolean
  , selectedVertex :: Maybe Int
  }

_graph :: forall a r. Lens' { graph :: a | r } a
_graph = prop (Proxy :: Proxy "graph")


initialState :: Context2D -> Element -> GlobalState
initialState ctx node =
  { graph: simpleGraph
  , ctx: ctx
  , mousePos: Nothing
  , mouseHeld: false
  , dragging: false
  , canvas: node
  , selectedVertex: Nothing
  }

simpleGraph :: GraphState
simpleGraph = newGraph
  ( M.fromFoldable $ map (\r -> Tuple r.label (newNode r.nodeType r.pos))
      [ {label: 0, nodeType: Binary, pos: (Vec 100.0 200.0)}
      , {label: 1, nodeType: Binary, pos: (Vec 200.0 200.0)}
      , {label: 2, nodeType: Nullary, pos: (Vec 200.0 400.0)}
      , {label: 3, nodeType: Nullary, pos: (Vec 500.0 500.0)}
      ]
  )
  ( S.fromFoldable
    [ makeEdge 0 1 LeftPort RightPort
    , makeEdge 1 2 LeftPort PrimaryPort
    , makeEdge 1 0 PrimaryPort PrimaryPort
    , makeEdge 3 1 PrimaryPort RightPort
    ]
  )
  where
  newNode nodeType x = {nodeType, particle: defaultParticle x}
  defaultParticle x = { x: x, v: zero, m: 1.0 }
  makeEdge i1 i2 p1 p2 = Edge (PortConnection i1 p1) (PortConnection i2 p2)