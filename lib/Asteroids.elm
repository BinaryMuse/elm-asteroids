module Asteroids where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing (float, generate, initialSeed)

import Colors exposing (..)
import GameConfig exposing (GameOptions, gameOptions)
import Math.Vector2 as Vector2 exposing (Vec2, vec2, getX, getY)
import Util exposing (angleToVector, wrapPosition, tileForm)

type Size = Large | Medium | Small

type alias Asteroid =
  { angle: Float
  , position: Vec2
  , size: Size
  }

type alias Model = List (Asteroid)

type alias Update = Float

generateModel count =
  List.repeat count (generateAsteroid)

generateAsteroid =
  let
    rndX = float -800 800
    rndY = float -400 400
    rndAngle = float -400 400
    -- rndAngle = float 0 3.14159
    (x, _) = generate rndX (initialSeed 5)
    (y, _) = generate rndY (initialSeed 5)
    (angle, _) = generate rndAngle (initialSeed 5)
  in
    { angle = angle
    , position = vec2 x y
    , size = Large
    }

update : Float -> Model -> Model
update delta model =
  List.map (updateSingle delta) model

updateSingle : Float -> Asteroid -> Asteroid
updateSingle delta asteroid =
  asteroid
    |> movement delta gameOptions

movement : Float -> GameOptions -> Asteroid -> Asteroid
movement delta gameOptions asteroid =
  asteroid

view : (Int, Int) -> Model -> Element
view (w, h) model =
  collage w h
    [ List.map drawAsteroid model |> group
    ]

drawAsteroid asteroid =
  circle 20
    |> filled white
    |> tileForm ((getX asteroid.position), (getY asteroid.position)) (1600, 800)

input : Signal Float -> Signal Update
input clock =
  Signal.map (\x -> x) clock
