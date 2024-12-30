module Constants
  ( SpringConsts
  , edgeColor
  , nodeColor
  , nodeRadius
  , portColor
  , selectedNodeColor
  , springConsts
  ) where

import Geometry (Field)

nodeColor :: String
nodeColor = "#ffaa00"

portColor :: String
portColor = "#b0aaff"

selectedNodeColor :: String
selectedNodeColor = "#00d0ff"

edgeColor :: String
edgeColor = "#ffffff"

nodeRadius :: Number
nodeRadius = 10.0

type SpringConsts =
  { k :: Field
  , dx :: Field
  , drag :: Field
  , q :: Field
  , minElectricalDistance :: Field
  }

springConsts :: SpringConsts
springConsts =
  { k: 0.01
  , dx: 200.0
  , drag: 10.0
  , q: 100.0
  , minElectricalDistance: 5.0
  }