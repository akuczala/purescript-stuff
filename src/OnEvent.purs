module OnEvent
  ( onEvent
  ) where

import Prelude

import Constants (springConsts)
import Control.Monad.State (StateT, get, modify)
import Data.Graph (addVertex, modifyVertex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..))
import Draw (render)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Events (MyEvent(..))
import Geometry (Point)
import Springy (updateNetwork)
import State (GlobalState)
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
      _ <- modify (\s -> s { mousePos = Just newMousePos })
      pure unit
    Nothing -> pure unit

onEvent (MouseDown _) = do
  _ <- modify (\s -> s { mouseHeld = true })
  pure unit

onEvent (MouseUp _) = do
  _ <- modify (\s -> s { mouseHeld = false })
  pure unit

onEvent (KeyDown e) = do
  case map key (KB.fromEvent e) of
    Just "c" -> onCreateNodeEvent
    _ -> pure unit

onEvent (Wheel _) = pure unit

onEvent (Draw) = do
  state <- get
  _ <- liftEffect $ render state
  _ <- modify update
  pure unit

onCreateNodeEvent :: forall m. Monad m => StateT GlobalState m Unit
onCreateNodeEvent = do
  _ <- modify
    ( \s ->
        case s.mousePos of
          Just mPos -> s { graph = addVertex { x: mPos, v: zero, m: 1.0 } s.graph }
          Nothing -> s
    )
  pure unit

update :: GlobalState -> GlobalState
update state = testMoveNode $ state
  { graph = updateNetwork springConsts 0.005 state.graph
  }

testMoveNode :: GlobalState -> GlobalState
testMoveNode s = case Tuple s.mouseHeld s.mousePos of
  Tuple true (Just mpos) -> s
    { graph = modifyVertex 0 (\p -> p { x = mpos }) s.graph
    }
  _ -> s