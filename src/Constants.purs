module Constants
  ( edgeColor
  , nodeColor
  , nodeRadius
  , selectedNodeColor
  , springConsts
  ) where

import Springy (SpringConsts)

nodeColor :: String
nodeColor = "#ffaa00"

selectedNodeColor :: String
selectedNodeColor = "#00d0ff"

edgeColor :: String
edgeColor = "#ffffff"

nodeRadius :: Number
nodeRadius = 10.0

springConsts :: SpringConsts
springConsts = { k: 0.01, dx: 100.0, drag: 5.0 }