module Main
  ( main
  , render
  )
  where

import Prelude

import Data.Graph (Edge(..), Graph, newGraph)
import Data.Int (floor)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..))
import Draw (drawNetwork)
import Effect (Effect)
import Effect.Timer (setTimeout)
import Graphics.Canvas (Context2D, clearRect, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle)
import Partial.Unsafe (unsafePartial)
import Springy (Particle, SpringConsts, updateNetwork)
import Web.DOM (Element)
import Web.DOM.Document (toParentNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window, Window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document, requestAnimationFrame)

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
springConsts = {k: 0.01, dx: 100.0, drag: 1.0}

update :: forall k. Ord k => (Graph k Particle) -> (Graph k Particle)
update = updateNetwork springConsts 0.01 

render :: forall k. Ord k => (Graph k Particle) -> Context2D -> Window -> Effect Unit
render g ctx w = do
  clearRect ctx {x: 0.0, y: 0.0, width: 800.0, height: 800.0}
  setFillStyle ctx "#ffaa00"
  setStrokeStyle ctx "#ffffff"
  drawNetwork ctx $ map _.x g
  _ <- setTimeout framePeriod $ do
    _ <- requestAnimationFrame (render (update g) ctx w) w
    pure unit
  pure unit

main :: Effect Unit
main = do
  w <- window
  ctx <- getCanvas
  render simpleGraph ctx w

simpleGraph :: Graph Int Particle
simpleGraph = map (\x -> {x: x, v: zero, m: 1.0}) $ newGraph (
  M.fromFoldable [
    Tuple 0 (Vec 100.0 200.0),
    Tuple 1 (Vec 200.0 200.0),
    Tuple 2 (Vec 200.0 400.0),
    Tuple 3 (Vec 500.0 500.0)
    ]
  ) (
    S.fromFoldable [Edge 0 1, Edge 1 2, Edge 2 0, Edge 2 3]
    )