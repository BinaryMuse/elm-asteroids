import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Math.Vector2 as Vector2 exposing (vec2, Vec2)
import Text exposing (fromString)

-- Game config

-- Set to True to stop immediately upon releaseing up key
friction : Bool
friction = False

-- How fast the ship accelerates; is much higher with friction on
-- since the velocity doesn't increase over time
acceleration : Float
acceleration = if friction then 5 else 0.2

-- How often a bullet can be fired if the user holds down space
bulletFireRate : Float
bulletFireRate = 200

-- How long a bullet will be alive before it disappears
bulletTimeAlive : Float
bulletTimeAlive = 3000

bulletMovementRate : Float
bulletMovementRate = 0.5

turnRate : Float
turnRate = 0.005


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
  , deltaSinceLastBullet = bulletFireRate + 1
  , bullets = []
  }

update : (Float, Keys, Bool) -> Model -> Model
update (delta, keys, space) model =
  model
    |> turning delta keys
    |> movement delta keys
    |> bullets delta space

turning : Float -> Keys -> Model -> Model
turning delta keys model =
  { model |
    angle = model.angle - turnRate * delta * (toFloat keys.x)
  }

movement : Float -> Keys -> Model -> Model
movement delta keys model =
  let
    keysY = max 0 (toFloat keys.y)
    vector = getVectorFromAngle model.angle
    newVelocity = vector
      |> Vector2.scale (keysY * acceleration)
      |> Vector2.add model.velocity
    newPosition = model.position
      |> Vector2.add newVelocity
      |> wrapPosition
  in
    { model |
      position = newPosition
    , velocity = if friction then model.velocity else newVelocity
    }

bullets : Float -> Bool -> Model -> Model
bullets delta space model =
  model
    |> advanceBullets delta
    |> fireBullet delta space

advanceBullets : Float -> Model -> Model
advanceBullets delta model =
  { model |
    bullets = model.bullets
                |> List.map (advanceBullet delta)
                |> List.filter (\b -> b.timeAlive <= bulletTimeAlive)
  }

advanceBullet : Float -> Bullet -> Bullet
advanceBullet delta bullet =
  let
    newTimeAlive = bullet.timeAlive + delta
    vector = getVectorFromAngle bullet.angle
    moved = Vector2.scale (bulletMovementRate * newTimeAlive) vector
    finalPos = Vector2.add bullet.startPosition moved
  in
    { bullet |
      timeAlive = newTimeAlive
    , currentPosition = finalPos
    }

fireBullet : Float -> Bool -> Model -> Model
fireBullet delta space model =
  if space && model.deltaSinceLastBullet + delta >= bulletFireRate then
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

getVectorFromAngle : Float -> Vec2
getVectorFromAngle angle =
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
