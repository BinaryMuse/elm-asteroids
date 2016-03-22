module Ship where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard

import Math.Vector2 as Vector2 exposing (Vec2, vec2, getX, getY)

import Colors exposing (..)
import GameConfig exposing (GameOptions, gameOptions)
import Util exposing (angleToVector, wrapPosition, tileForm)

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
  }

type alias Update = (Float, Keys, Bool)

model : Model
model =
  { angle = 0
  , position = vec2 0 0
  , velocity = vec2 0 0
  , gunCooldownRemaining = 0
  , bullets = []
  }

update : Update -> Model -> Model
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
  { bullet |
    timeAlive = bullet.timeAlive + delta
  , position = bullet.angle
      |> Util.angleToVector
      |> Vector2.scale (gameOptions.bulletMovementRate * delta)
      |> Vector2.add bullet.position
  }

fireBullet : Float -> GameOptions -> Bool -> Model -> Model
fireBullet delta gameOptions space model =
  if space && model.gunCooldownRemaining - delta <= 0 then
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
      { model | gunCooldownRemaining = max 0 (model.gunCooldownRemaining - delta) }

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

input : Signal Float -> Signal Update
input clock =
  Signal.map3 (,,) clock Keyboard.arrows Keyboard.space
