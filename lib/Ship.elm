module Ship where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (fromString)

import Math.Vector2 as Vector2 exposing (vec2, Vec2)

import GameConfig exposing (GameOptions, gameOptions)
import Util exposing (angleToVector, wrapPosition, tileForm)

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
  , gunCooldownRemaining: Float
  , bullets: List (Bullet)
  }

model : Model
model =
  { angle = 0
  , position = vec2 0 0
  , velocity = vec2 0 0
  , gunCooldownRemaining = 0
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
  if space && model.gunCooldownRemaining - delta <= 0 then
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
      , gunCooldownRemaining = gameOptions.gunCooldownTime
      }
  else
      { model | gunCooldownRemaining = max 0 (model.gunCooldownRemaining - delta) }

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
