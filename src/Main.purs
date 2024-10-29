module Main
  ( main
  , render
  ) where

import Prelude

import Control.Monad.State (StateT, get, modify, runStateT)
import Data.Graph (Edge(..), Graph, addVertex, modifyVertex, newGraph)
import Data.Int (floor, toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..))
import Draw (drawNetwork)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Events (MyEvent(..), eventProducer, inputConsumer, setupEventLoop)
import Graphics.Canvas (Context2D, clearRect, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle)
import Partial.Unsafe (unsafePartial)
import Springy (Particle, SpringConsts, Point, updateNetwork)
import Web.DOM (Element)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (getBoundingClientRect, toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (key)
import Web.UIEvent.KeyboardEvent as KB
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent)

getCanvas :: Effect Context2D
getCanvas = unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  getContext2D canvas

getCanvasNode :: Effect (Maybe Element)
getCanvasNode = do
  doc <- map (toParentNode <<< toDocument) (document =<< window)
  querySelector (QuerySelector "#canvas") doc

frameRate :: Number
frameRate = 60.0

framePeriod :: Int
framePeriod = floor $ 1000.0 / frameRate

springConsts :: SpringConsts
springConsts = { k: 0.01, dx: 100.0, drag: 5.0 }

update :: forall k. Ord k => (Graph k Particle) -> (Graph k Particle)
update = updateNetwork springConsts 0.005

render :: forall k. Ord k => (Graph k Particle) -> Context2D -> Effect Unit
render g ctx = do
  clearRect ctx { x: 0.0, y: 0.0, width: 800.0, height: 800.0 }
  setFillStyle ctx "#ffaa00"
  setStrokeStyle ctx "#ffffff"
  drawNetwork ctx $ map _.x g

type GlobalState =
  { graph :: Graph Int Particle
  , ctx :: Context2D
  , canvas :: Element
  , mousePos :: Maybe Point
  , selectedVertex :: Maybe Int
  }

initialState :: Context2D -> Element -> GlobalState
initialState ctx node =
  { graph: simpleGraph
  , ctx: ctx
  , mousePos: Nothing
  , canvas: node
  , selectedVertex: Nothing
  }

main :: Effect Unit
main = do
  ctx <- getCanvas
  maybeCanvasNode <- getCanvasNode
  -- testKey
  case maybeCanvasNode of
    Just canvasNode -> do
      launchAff_ $ do
        _ <- runStateT (setup canvasNode) (initialState ctx canvasNode)
        pure unit
    Nothing -> pure unit

setup :: Element -> StateT GlobalState Aff Unit
setup el = do
  setupEventLoop (inputConsumer onEvent) (eventProducer framePeriod $ toEventTarget el)

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
  _ <- modify
    ( \s -> case s.mousePos of
        Just mpos -> s
          { graph = modifyVertex 0 (\p -> p { x = mpos }) s.graph
          }
        Nothing -> s
    )
  pure unit

onEvent (KeyDown e) = do
  case map key (KB.fromEvent e) of
    Just "c" -> onCreateNodeEvent
    _ -> pure unit

onEvent (Wheel _) = pure unit

onEvent (Draw) = do
  state <- get
  _ <- liftEffect $ render state.graph state.ctx
  _ <- modify (\s -> s { graph = update s.graph })
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

simpleGraph :: Graph Int Particle
simpleGraph = map (\x -> { x: x, v: zero, m: 1.0 }) $ newGraph
  ( M.fromFoldable
      [ Tuple 0 (Vec 100.0 200.0)
      , Tuple 1 (Vec 200.0 200.0)
      , Tuple 2 (Vec 200.0 400.0)
      , Tuple 3 (Vec 500.0 500.0)
      ]
  )
  ( S.fromFoldable [ Edge 0 1, Edge 1 2, Edge 2 0, Edge 2 3 ]
  )