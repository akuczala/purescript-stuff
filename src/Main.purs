module Main
  ( main
  , render
  )
  where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (range, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Timer (setTimeout)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D, setFillStyle)
import Partial.Unsafe (unsafePartial)
import Rectangle (drawRects, rectangle_main, xyPairs)
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
render :: Boolean -> Context2D -> Window -> Effect Unit
render b ctx w = do
  setFillStyle ctx (colorz b)
  drawRects ctx xyPairs
  _ <- setTimeout 200 $ do
    _ <- requestAnimationFrame (render (not b) ctx w) w
    pure unit
  pure unit

main :: Effect Unit
main = do
  w <- window
  ctx <- getCanvas
  render false ctx w