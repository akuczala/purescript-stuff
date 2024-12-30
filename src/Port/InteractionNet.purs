module Port.InteractionNet
  ( IPort(..)
  , NodeType(..)
  )
  where

import Prelude

data IPort = LeftPort | RightPort | PrimaryPort

derive instance Eq IPort
derive instance Ord IPort

data NodeType = Binary | Nullary