module Ship where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time

import Math.Vector2 as Vector2 exposing (Vec2, vec2, getX, getY)

import Colors exposing (..)
import GameConfig exposing (GameOptions, gameOptions)
import GameTypes exposing (Tick, Input)
import Util exposing (angleToVector, wrapPosition, tileForm)

type Direction = Left | Right

type alias Bullet =
  { timeAlive: Float -- how long since this bullet was fired
  , angle: Float -- the angle of the ship at the time the bullet was fired
  , position: Vec2 -- the location of the bullet
  }

type alias Keys = { x: Int, y: Int }

type alias Model =
  { angle: Float
  , position: Vec2
  , velocity: Vec2
  , gunCooldownRemaining: Float
  , bullets: List (Bullet)
  , turningDirection: Maybe (Direction)
  , accelerating: Bool
  }

model : Model
model =
  { angle = 0
  , position = vec2 0 0
  , velocity = vec2 0 0
  , gunCooldownRemaining = 0
  , bullets = []
  , turningDirection = Nothing
  , accelerating = False
  }

update : Input -> Model -> Model
update {x, y, space} model =
  model
    |> turning gameOptions x
    |> accelerating gameOptions y
    |> firing gameOptions space

tick : Tick -> Model -> Model
tick delta model =
  model
    |> movement delta gameOptions
    |> bullets delta gameOptions

turning : GameOptions -> Int -> Model -> Model
turning gameOptions dir model =
  if dir == 1 then
    { model | turningDirection = Just Right }
  else if dir == -1 then
    { model | turningDirection = Just Left }
  else
    { model | turningDirection = Nothing }

accelerating : GameOptions -> Int -> Model -> Model
accelerating gameOptions y model =
  if y == 1 then
    { model | accelerating = True }
  else
    { model | accelerating = False }

movement : Float -> GameOptions -> Model -> Model
movement delta gameOptions model =
  model
    |> doTurn delta gameOptions
    |> doMove delta gameOptions

doTurn : Tick -> GameOptions -> Model -> Model
doTurn delta gameOptions model =
  let
    multiplier = case model.turningDirection of
      Just Left  -> -1
      Just Right -> 1
      Nothing    -> 0
    angle = model.angle - gameOptions.turnRate * multiplier
  in
    { model | angle = angle }

doMove : Tick -> GameOptions -> Model -> Model
doMove delta gameOptions model =
  let
    multiplier = if model.accelerating then 1 else 0
    heading = model.angle
      |> angleToVector
    proposedVelocity = heading
      |> Vector2.scale (multiplier * gameOptions.thrust)
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

firing : GameOptions -> Bool -> Model -> Model
firing gameOptions space model =
  model
    |> fireBullet gameOptions space

bullets : Float -> GameOptions -> Model -> Model
bullets delta gameOptions model =
  model
    |> advanceBullets delta gameOptions

advanceBullets : Float -> GameOptions -> Model -> Model
advanceBullets delta gameOptions model =
  { model |
    gunCooldownRemaining = max 0 (model.gunCooldownRemaining - delta)
  , bullets = model.bullets
                |> List.map (advanceBullet delta gameOptions)
                |> List.filter (\b -> b.timeAlive <= gameOptions.bulletTtl)
  }

advanceBullet : Float -> GameOptions -> Bullet -> Bullet
advanceBullet delta gameOptions bullet =
  { bullet |
    timeAlive = bullet.timeAlive + delta
  , position = bullet.angle
      |> Util.angleToVector
      |> Vector2.scale (gameOptions.bulletMovementRate * delta)
      |> Vector2.add bullet.position
  }

fireBullet : GameOptions -> Bool -> Model -> Model
fireBullet gameOptions space model =
  if space && model.gunCooldownRemaining <= 0 then
    let
      bullet =
        { timeAlive = 0
        , angle = model.angle
        , position = model.position
        }
    in
      { model |
        bullets = bullet :: model.bullets
      , gunCooldownRemaining = gameOptions.gunCooldownTime
      }
  else
    model

view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    w' = toFloat w
    h' = toFloat h
  in
    collage w h
      [ rect 1600 800
        |> filled black
      , polygon [(-10, 10), (20, 0), (-10, -10)]
        |> filled white
        |> rotate (radians model.angle)
        |> tileForm ((getX model.position), (getY model.position)) (1600, 800)
      , List.map drawBullet model.bullets |> group
      ]

drawBullet : Bullet -> Form
drawBullet bullet =
  circle 2
    |> filled white
    |> tileForm ((getX bullet.position), (getY bullet.position)) (1600, 800)

combineKeys : { x: Int, y: Int } -> Bool -> Input
combineKeys {x,y} spc = { x = x, y = y, space = spc }

input : Signal Input
input =
  Signal.map2 combineKeys Keyboard.arrows Keyboard.space
    |> Signal.sampleOn (Time.fps 30)
