module Port.OnEvent
  ( onEvent
  ) where

import Prelude

import Constants (nodeRadius, springConsts)
import Control.Alternative (guard)
import Control.Monad.Reader (runReader)
import Control.Monad.State (StateT, get, modify, modify_, put)
import Data.Graph (_verts, modifyVertex, toMap)
import Data.Int (toNumber)
import Data.Lens (over, traverseOf)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Events (MyEvent(..))
import Geometry (Field, Point, closestPoint, distance)
import Port.Draw (render)
import Port.State (GlobalState, NodeParticle, _graph, _particle, particleToPos)
import PortGraph (toGraph)
import Springy (updateParticle)
import Web.DOM (Element)
import Web.DOM.Element (getBoundingClientRect)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (key)
import Web.UIEvent.KeyboardEvent as KB
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent)

getMousePos :: Event -> Maybe Point
getMousePos e = do
  m <- fromEvent e
  pure $ Vec (toNumber $ clientX m) (toNumber $ clientY m)

toCanvasPos :: Element -> Point -> Effect Point
toCanvasPos e (Vec mx my) = do
  rect <- getBoundingClientRect e
  pure $ Vec (mx - rect.left) (my - rect.top)

onEvent :: MyEvent -> StateT GlobalState Aff Unit
onEvent (MouseMove e) = do
  case getMousePos e of
    Just mousePos -> do
      state <- get
      newMousePos <- liftEffect $ (toCanvasPos state.canvas) mousePos
      modify_ (\s -> s { mousePos = Just newMousePos })
    Nothing -> pure unit

onEvent (MouseDown _) = do
  state <- modify selectNode
  put state { mouseHeld = true }

onEvent (MouseUp _) = do
  modify_ (\s -> s { mouseHeld = false, dragging = false })

onEvent (KeyDown e) = do
  case map key (KB.fromEvent e) of
    --Just "c" -> modify_ createNode
    --Just "e" -> modify_ createEdge
    _ -> pure unit

onEvent (Wheel _) = pure unit

onEvent (Draw) = do
  state <- get
  liftEffect $ render state
  modify_ update

nodeCloseToMouse :: Field -> GlobalState -> Maybe (Tuple Int Point)
nodeCloseToMouse minDist state = do
  mpos <- state.mousePos
  (Tuple label v) <- closestPoint (toMap $ map (_.pos <<< particleToPos) state.graph) mpos
  guard $ distance mpos v <= minDist
  pure (Tuple label v)

selectNode :: GlobalState -> GlobalState
selectNode state = fromMaybe state do
  guard $ not state.mouseHeld
  (Tuple label _) <- nodeCloseToMouse nodeRadius state
  pure $ state { selectedVertex = Just label }

-- createNode :: GlobalState -> GlobalState
-- createNode state = fromMaybe state do
--   mPos <- state.mousePos
--   pure state
--     { graph = addVertex { x: mPos, v: zero, m: 1.0 } state.graph
--     }

-- createEdge :: GlobalState -> GlobalState
-- createEdge state = fromMaybe state do
--   selectedLabel <- state.selectedVertex
--   (Tuple targetLabel _) <- nodeCloseToMouse nodeRadius state
--   guard $ selectedLabel /= targetLabel
--   pure state
--     { graph = addEdge (Edge targetLabel selectedLabel) state.graph
--     }

update :: GlobalState -> GlobalState
update state = dragNode $ over (_graph <<< _verts) updateGraph state
  where
  particleGraph = (map _.particle) $ toGraph state.graph
  updateGraph :: Map Int NodeParticle -> Map Int NodeParticle
  updateGraph m = runReader (traverseWithIndex updateNode m) springConsts
  updateNode label = traverseOf _particle (updateParticle 0.005 particleGraph label)

dragNode :: GlobalState -> GlobalState
dragNode s = fromMaybe s do
  guard s.mouseHeld
  selectedLabel <- s.selectedVertex
  if s.dragging then do
    mpos <- s.mousePos
    pure s
      { graph = modifyVertex selectedLabel (\n -> n { particle = n.particle{x = mpos} }) s.graph
      , dragging = true
      }
  else do
    (Tuple closestLabel _) <- nodeCloseToMouse nodeRadius s
    guard $ selectedLabel == closestLabel
    pure s
      { dragging = true
      }