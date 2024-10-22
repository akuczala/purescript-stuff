module Rectangle
  ( rectangle_main,
    drawRects,
    xyPairs
  )
  where

import Prelude

import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, fillPath, getCanvasElementById, getContext2D, rect, setFillStyle)
import Partial.Unsafe (unsafePartial)
type Point = {x :: Number, y :: Number}

rectangle_main :: String -> Effect Unit
rectangle_main color = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx color

  drawRects ctx xyPairs

xyPairs :: Array Point
xyPairs = do
  x <- [100, 200, 300]
  y <- [100, 200, 300]
  pure $ {x: toNumber x, y: toNumber y}
  
  
drawRects :: Context2D -> Array Point -> Effect Unit
drawRects ctx arr = traverse_ f arr
  where f p = fillPath ctx $ rect ctx {
      x: p.x,
      y: p.y,
      width: 50.0,
      height: 50.0
    }