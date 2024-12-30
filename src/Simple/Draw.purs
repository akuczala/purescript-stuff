module Simple.Draw
  (
  drawNetwork
  , drawNode
  , render
  ) where

import Prelude

import Constants (edgeColor, nodeColor, nodeRadius, selectedNodeColor)
import Data.Foldable (traverse_)
import Data.Graph (lookup)
import Data.Graph as G
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Data.Vector2 as V
import Draw (drawCircle, drawLine)
import Effect (Effect)
import Geometry (Point)
import Graphics.Canvas (Context2D, clearRect, fillPath, setFillStyle, setStrokeStyle)
import Simple.State (GlobalState)

drawNetwork :: forall k. Ord k => Context2D -> (G.Graph k Point) -> Effect Unit
drawNetwork ctx g = do
  traverse_ drawEdge (G.getEdges g)
  traverse_ (snd >>> drawNode ctx) (G.getVerts g :: Array (Tuple k Point))
  where
  vertMap = G.toMap g
  drawEdge (G.Edge k1 k2) = case [ M.lookup k1 vertMap, M.lookup k2 vertMap ] of
    [ Just p1, Just p2 ] -> drawLine ctx p1 p2
    _ -> pure unit

drawNode :: Context2D -> Point -> Effect Unit
drawNode ctx p = fillPath ctx $ drawCircle ctx
  { p
  , radius: nodeRadius
  }

render :: GlobalState -> Effect Unit
render s = do
  clearRect s.ctx { x: 0.0, y: 0.0, width: 800.0, height: 800.0 }
  setFillStyle s.ctx nodeColor
  setStrokeStyle s.ctx edgeColor
  drawNetwork s.ctx pointGraph
  drawSelected s.selectedVertex
  where
  pointGraph = map _.x s.graph
  drawSelected Nothing = pure unit
  drawSelected (Just selectedVertex) =
    case lookup selectedVertex pointGraph of
      Nothing -> pure unit
      Just v -> do
        setFillStyle s.ctx selectedNodeColor
        drawNode s.ctx v