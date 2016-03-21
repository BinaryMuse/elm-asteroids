module Util where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Math.Vector2 as Vector2 exposing (vec2, Vec2)

-- Data utils

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

-- Graphics utils

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
