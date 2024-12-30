module Draw where

import Prelude

import Data.Number as Math
import Data.Vector2 (getX, getY)
import Data.Vector2 as V
import Effect (Effect)
import Geometry (Point)
import Graphics.Canvas (Context2D, arc, fillPath, lineTo, moveTo, strokePath)

drawCircle
  :: Context2D
  -> { p :: Point, radius :: Number }
  -> Effect Unit
drawCircle ctx rec = arc ctx $
  { x: getX rec.p
  , y: getY rec.p
  , radius: rec.radius
  , start: 0.0
  , end: Math.tau
  , useCounterClockwise: false
  }

drawLine :: Context2D -> Point -> Point -> Effect Unit
drawLine ctx p1 p2 = strokePath ctx $ do
  moveTo ctx (V.getX p1) (V.getY p1)
  lineTo ctx (V.getX p2) (V.getY p2)

drawTriangle :: Context2D -> Point -> Point -> Point -> Effect Unit
drawTriangle ctx p1 p2 p3 = fillPath ctx $ do
  moveTo ctx (V.getX p1) (V.getY p1)
  lineTo ctx (V.getX p2) (V.getY p2)
  lineTo ctx (V.getX p3) (V.getY p3)
