import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Math.Vector2 as Vector2 exposing (vec2, Vec2)
import Text exposing (fromString)

-- Game config

type alias GameOptions =
  { friction: Bool -- if true, ship stops immediately
  , thrust: Float -- how quickly the ship can increase momentum
  , maxSpeed: Float -- the maximum speed at which the ship can travel
  , bulletFireRate: Float -- how quickly the gun can fire
  , bulletTtl: Float -- how long bullets last
  , bulletMovementRate: Float -- how fast bulltes move
  , turnRate: Float -- how qickly teh ship can turn
  }

gameOptions =
  let
    friction = False
  in
    { friction = friction
    , thrust = if friction then 5 else 1
    , maxSpeed = 30
    , bulletFireRate = 200
    , bulletTtl = 3000
    , bulletMovementRate = 0.5
    , turnRate = 0.005
    }


-- Game data

type alias Bullet =
  { timeAlive: Float
  , angle: Float
  , startPosition: Vec2
  , currentPosition: Vec2
  }

type alias Keys = { x: Int, y: Int }

type alias Model =
  { angle: Float
  , position: Vec2
  , velocity: Vec2
  , deltaSinceLastBullet: Float
  , bullets: List (Bullet)
  }

model : Model
model =
  { angle = 0
  , position = vec2 0 0
  , velocity = vec2 0 0
  , deltaSinceLastBullet = gameOptions.bulletFireRate + 1
  , bullets = []
  }

update : (Float, Keys, Bool) -> Model -> Model
update (delta, keys, space) model =
  model
    |> turning delta gameOptions keys
    |> movement delta gameOptions keys
    |> bullets delta gameOptions space

turning : Float -> GameOptions -> Keys -> Model -> Model
turning delta gameOptions keys model =
  { model |
    angle = model.angle - gameOptions.turnRate * delta * (toFloat keys.x)
  }

movement : Float -> GameOptions -> Keys -> Model -> Model
movement delta gameOptions keys model =
  let
    keysY = max 0 (toFloat keys.y)
    heading = model.angle
      |> angleToVector
    proposedVelocity = heading
      |> Vector2.scale (keysY * gameOptions.thrust)
      |> Vector2.add model.velocity
    newVelocity = if (Vector2.length proposedVelocity) > gameOptions.maxSpeed then
        heading |> Vector2.scale gameOptions.maxSpeed
      else
        proposedVelocity
    newPosition = model.position
      |> Vector2.add newVelocity
      |> wrapPosition
  in
    { model |
      position = newPosition
    , velocity = if gameOptions.friction then model.velocity else newVelocity
    }

bullets : Float -> GameOptions -> Bool -> Model -> Model
bullets delta gameOptions space model =
  model
    |> advanceBullets delta gameOptions
    |> fireBullet delta gameOptions space

advanceBullets : Float -> GameOptions -> Model -> Model
advanceBullets delta gameOptions model =
  { model |
    bullets = model.bullets
                |> List.map (advanceBullet delta gameOptions)
                |> List.filter (\b -> b.timeAlive <= gameOptions.bulletTtl)
  }

advanceBullet : Float -> GameOptions -> Bullet -> Bullet
advanceBullet delta gameOptions bullet =
  let
    newTimeAlive = bullet.timeAlive + delta
    vector = angleToVector bullet.angle
    moved = Vector2.scale (gameOptions.bulletMovementRate * newTimeAlive) vector
    finalPos = Vector2.add bullet.startPosition moved
  in
    { bullet |
      timeAlive = newTimeAlive
    , currentPosition = finalPos
    }

fireBullet : Float -> GameOptions -> Bool -> Model -> Model
fireBullet delta gameOptions space model =
  if space && model.deltaSinceLastBullet + delta >= gameOptions.bulletFireRate then
    let
      newBullet =
        { timeAlive = 0
        , angle = model.angle
        , startPosition = model.position
        , currentPosition = model.position
        }
      newBullets = newBullet :: model.bullets
    in
      { model |
        bullets = newBullets
      , deltaSinceLastBullet = 0
      }
  else
      { model | deltaSinceLastBullet = model.deltaSinceLastBullet + delta }

angleToVector : Float -> Vec2
angleToVector angle =
  let
    x = cos angle
    y = sin angle
  in
    Vector2.vec2 x y

wrapPosition : Vec2 -> Vec2
wrapPosition vec =
  let
    x = Vector2.getX vec
    y = Vector2.getY vec
    x' = if x > 800 then -800 else x
    y' = if y > 400 then -400 else y
    x'' = if x' < -800 then 800 else x'
    y'' = if y' < -400 then 400 else y'
  in
    vec2 x'' y''


-- Game view / loop

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update model input)

view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    w' = toFloat w
    h' = toFloat h
  in
    collage w h
      [ rect 1600 800
        |> filled (rgb 0 0 0)
      , polygon [(-10, 10), (20, 0), (-10, -10)]
        |> filled (rgb 255 255 255)
        |> rotate (radians model.angle)
        |> tileForm ((Vector2.getX model.position), (Vector2.getY model.position)) (1600, 800)
      , List.map drawBullet model.bullets |> group
      , scoreText model
        |> moveY 350
      ]

scoreText model =
  let
    lineStyle = { defaultLine |
      color = (rgb 255 255 255)
    , width = 1
    }
    t = Text.fromString "Score: 0"
          |> Text.monospace
          |> Text.height 30
          |> Text.color (rgb 255 255 255)
  in
    outlinedText lineStyle t

drawBullet : Bullet -> Form
drawBullet bullet =
  circle 2
    |> filled (rgb 255 255 255)
    |> tileForm ((Vector2.getX bullet.currentPosition), (Vector2.getY bullet.currentPosition)) (1600, 800)

tileForm : (Float, Float) -> (Float, Float) -> Form -> Form
tileForm pos limit form =
  getTilePositions pos limit
    |> List.map (\newPos -> move newPos form)
    |> group

getTilePositions (x, y) (maxX, maxY) =
  let
    newX = if x > 0 then x - maxX else x + maxX
    newY = if y > 0 then y - maxY else y + maxY
  in
    [ (x, y)
    , (newX, y)
    , (x, newY)
    , (newX, newY)
    ]

drawTile : Int -> (Float, Float) -> (Float, Float) -> Form -> Form
drawTile index (x, y) (maxX, maxY) form =
  let
    newX = if x > maxX / 2 then x - maxX else x + maxX
    newY = if y > maxY / 2 then y - maxY else y + maxY
  in
    if index == 0 then
      move (x, y) form
    else if index == 1 then
      move (newX, y) form
    else if index == 2 then
      move (x, newY) form
    else
      move (newX, newY) form

-- `input` is a Signal ...
input : Signal (Float, Keys, Bool)
input =
  let
    -- ... that updates every 33 ms ....
    delta = (fps 30)
  in
    -- ... that provides a tuple of the time since last update and the Keyboard info
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.space)
