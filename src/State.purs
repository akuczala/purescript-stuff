module State where

import Prelude

import Data.Graph (Edge(..), Graph, newGraph)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Vector2 (Vec(..))
import Geometry (Point)
import Graphics.Canvas (Context2D)
import Springy (Particle)
import Web.DOM (Element)

type GlobalState =
  { graph :: Graph Int Particle
  , ctx :: Context2D
  , canvas :: Element
  , mousePos :: Maybe Point
  , mouseHeld :: Boolean
  , dragging :: Boolean
  , selectedVertex :: Maybe Int
  }

initialState :: Context2D -> Element -> GlobalState
initialState ctx node =
  { graph: simpleGraph
  , ctx: ctx
  , mousePos: Nothing
  , mouseHeld: false
  , dragging: false
  , canvas: node
  , selectedVertex: Nothing
  }

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