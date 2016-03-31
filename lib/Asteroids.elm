module Asteroids where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing (Seed, float, generate, initialSeed)

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

type alias Model =
  { seed: Seed
  , asteroids: List (Asteroid) }

type alias Update = Float

-- floatGenerator = Signal.foldp \nextSeed seed ->  state Signal.Signal a

generateModel : Int -> Model
generateModel count =
  { seed = initialSeed 5
  , asteroids = [(buildAsteroid)] }
  -- let
  --   xGen = float -800 800
  --   yGen = float -400 400
  --   angleGen = float 0 (22 / 7)
  -- in
  --   List.repeat count (generateAsteroid (xGen, yGen, angleGen))

buildAsteroid =
  { angle = 0.3, position = (vec2 100 100), size = Large }

generateAsteroid (xGen, yGen, angleGen) =
  let
    (x, _) = generate xGen (initialSeed 5)
    (y, _) = generate yGen (initialSeed 5)
    (angle, _) = generate angleGen (initialSeed 5)
  in
    { angle = angle
    , position = vec2 x y
    , size = Large
    }

update : Float -> Model -> Model
update delta model =
  { model | asteroids = List.map (updateSingle delta) model.asteroids }

tick delta model =
  { model | asteroids = List.map (updateSingle delta) model.asteroids }

updateSingle : Float -> Asteroid -> Asteroid
updateSingle delta asteroid =
  asteroid
    |> movement delta gameOptions

movement : Float -> GameOptions -> Asteroid -> Asteroid
movement delta gameOptions asteroid =
  let
    position' = asteroid.angle
      |> angleToVector
      |> Vector2.scale 1
      |> Vector2.add asteroid.position
      |> wrapPosition
  in
    { asteroid | position = position' }

view : (Int, Int) -> Model -> Element
view (w, h) model =
  collage w h
    [ List.map drawAsteroid model.asteroids |> group
    ]

drawAsteroid : Asteroid -> Form
drawAsteroid asteroid =
  circle 20
    |> filled white
    |> tileForm ((getX asteroid.position), (getY asteroid.position)) (1600, 800)

input : Signal Float -> Signal Update
input clock =
  clock
