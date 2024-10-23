module Main
  ( main
  , render
  )
  where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (range, foldM)
import Data.Graph (Edge(..), Graph, newGraph)
import Data.Int (floor)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..), oneX)
import Draw (drawNetwork)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Timer (setTimeout)
import Graphics.Canvas (Context2D, clearRect, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle)
import Partial.Unsafe (unsafePartial)
import Springy (Point, SpringConsts, Particle, smul, updateNetwork)
import Web.HTML (window, Window)
import Web.HTML.ValidityState (valid)
import Web.HTML.Window (requestAnimationFrame, screenX)

-- animate :: ∀ a. Number -> (a -> Effect Unit) -> Array a -> Aff Unit
-- animate dt f arr = foldM go unit arr
--   where
--     go _ n = do
--       liftEffect $ f n
--       delay $ Milliseconds dt

getCanvas :: Effect Context2D
getCanvas = unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  getContext2D canvas

colorz :: Boolean -> String
colorz false = "#ffaa00"
colorz true = "#b300ff"

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
  setFillStyle ctx (colorz false)
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
    Tuple 2 (Vec 200.0 400.0)
    ]
  ) (
    S.fromFoldable [Edge 0 1, Edge 1 2, Edge 2 0]
    )