module Game where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (fps)

import Math.Vector2 as Vector2 exposing (Vec2, vec2, getX, getY)

import GameTypes
import GameConfig exposing (GameOptions, gameOptions)
import Asteroids
import Score
import Ship

type alias Model =
  { ship: Ship.Model
  , asteroids: Asteroids.Model
  }

model : Model
model =
  { ship = Ship.model
  , asteroids = Asteroids.generateModel 3
  }

update : GameTypes.Update -> Model -> Model
update update model =
  case update of
    GameTypes.TickUpdate delta ->
      { model |
        ship = Ship.tick delta model.ship
      , asteroids = Asteroids.tick delta model.asteroids }
    GameTypes.InputUpdate shipUpdate ->
      { model | ship = Ship.update shipUpdate model.ship }

view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    content = layers [
      collage 1600 800 [
        toForm <| Ship.view (w, h) model.ship
      , toForm <| Asteroids.view (w, h) model.asteroids
      ]
    , container 1600 800 (topRightAt (absolute 20) (absolute  20)) Score.view
    ]
  in
    container w h middle content

input : Signal GameTypes.Update
input =
  Signal.mergeMany [
    Signal.map GameTypes.InputUpdate Ship.input
  , Signal.map GameTypes.TickUpdate (fps 30)
  ]
