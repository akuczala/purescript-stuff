module Port.Draw
  ( drawNode
  , render
  )
  where

import Prelude

import Constants (edgeColor, nodeColor, portColor, selectedNodeColor)
import Data.Array as A
import Data.Foldable (traverse_)
import Data.Graph (lookup)
import Data.Graph as G
import Data.Lens (view)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi)
import Data.Tuple (Tuple, snd)
import Draw (drawCircle, drawLine, drawTriangle)
import Effect (Effect)
import Geometry (Point, circlePoints, smul)
import Graphics.Canvas (Context2D, clearRect, fillPath, setFillStyle, setStrokeStyle)
import Port.InteractionNet (IPort(..), NodeType(..))
import Port.State (GlobalState, NodePos, particleToPos)
import PortGraph (PortGraph, _nodeLabel, _port)

drawNetwork :: forall k
  . Ord k
  => Context2D
  -> PortGraph k IPort NodePos
  -> Effect Unit
drawNetwork ctx g = do
  traverse_ drawEdge (G.getEdges g)
  traverse_ (snd >>> drawNode ctx) (G.getVerts g :: Array (Tuple k NodePos))
  where
  vertMap = G.toMap g
  drawEdge (G.Edge pc1 pc2) = fromMaybe (pure unit) do
     n1 <- M.lookup (view _nodeLabel pc1) vertMap
     n2 <- M.lookup (view _nodeLabel pc2) vertMap
     pure $ drawLine ctx (portPos n1 (view _port pc1)) (portPos n2 (view _port pc2))

trianglePoints :: Array Point
trianglePoints = circlePoints {n: 3, angle: 3.0 * pi / 2.0}

portPos :: NodePos -> IPort -> Point
portPos {nodeType: Nullary, pos} _ = pos
portPos {nodeType: Binary, pos} port = (add pos) $ smul 50.0 $ fromMaybe zero $ case port of
  PrimaryPort -> (trianglePoints A.!! 0)
  LeftPort -> (trianglePoints A.!! 1)
  RightPort -> (trianglePoints A.!! 2)

drawNode :: Context2D -> NodePos -> Effect Unit
drawNode ctx n = case n.nodeType of
    Nullary -> fillPath ctx $ drawCircle ctx {p: n.pos, radius: 10.0}
    Binary -> do
      let points = map (\p -> smul 50.0 p + n.pos) $ trianglePoints
      setFillStyle ctx portColor
      traverse_ (\p -> fillPath ctx $ drawCircle ctx {p, radius: 10.0}) points
      case A.take 3 points of
        [a, b, c] -> setFillStyle ctx nodeColor *> drawTriangle ctx a b c
        _ -> pure unit

render :: GlobalState -> Effect Unit
render s = do
  clearRect s.ctx { x: 0.0, y: 0.0, width: 800.0, height: 800.0 }
  setFillStyle s.ctx nodeColor
  setStrokeStyle s.ctx edgeColor
  drawNetwork s.ctx pointGraph
  drawSelected s.selectedVertex
  where
  pointGraph = map particleToPos s.graph :: PortGraph Int IPort NodePos
  drawSelected Nothing = pure unit
  drawSelected (Just selectedVertex) =
    case lookup selectedVertex pointGraph of
      Nothing -> pure unit
      Just v -> do
        setFillStyle s.ctx selectedNodeColor
        drawNode s.ctx v