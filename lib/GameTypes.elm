module GameTypes where

type alias Tick = Float
type alias Input =
  { x: Int
  , y: Int
  , space: Bool
  }

type Update
  = TickUpdate Tick
  | InputUpdate Input
