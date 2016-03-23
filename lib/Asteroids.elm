module Asteroids where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

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
    [ circle 20
      |> filled white
    ]

input : Signal Float -> Signal Update
input clock =
  Signal.map (\x -> x) clock
