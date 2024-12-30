module Main
  ( main
  ) where

import Prelude

import Control.Monad.State (StateT, runStateT)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Events (eventProducer, inputConsumer, setupEventLoop)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Port.OnEvent (onEvent)
import Partial.Unsafe (unsafePartial)
import Port.State (GlobalState, initialState)
import Web.DOM (Element)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

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
