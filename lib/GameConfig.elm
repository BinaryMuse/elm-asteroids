module GameConfig where

type alias GameOptions =
  { friction: Bool -- if true, ship stops immediately
  , thrust: Float -- how quickly the ship can increase momentum
  , maxSpeed: Float -- the maximum speed at which the ship can travel
  , gunCooldownTime: Float -- how quickly the gun can fire
  , bulletTtl: Float -- how long bullets last
  , bulletMovementRate: Float -- how fast bulltes move
  , turnRate: Float -- how qickly the ship can turn
  }

gameOptions =
  let
    friction = False
  in
    { friction = friction
    , thrust = if friction then 5 else 1
    , maxSpeed = 30
    , gunCooldownTime = 200
    , bulletTtl = 3000
    , bulletMovementRate = 0.5
    , turnRate = 0.005
    }
