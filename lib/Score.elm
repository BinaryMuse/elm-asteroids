module Score where
import Debug

import Graphics.Collage exposing (defaultLine, outlinedText, toForm)
import Graphics.Element exposing (rightAligned)
import Text exposing (fromString)

import Colors exposing (..)
import GameTypes exposing (Tick, Input)

type alias Model =
  { score: Int
  , timeSinceLastTick: Float }

model : Model
model =
  { score = 0, timeSinceLastTick = 0 }

tick : Tick -> Model -> Model
tick delta model =
  model
    |> incrementTimer delta
    |> incrementScore

incrementTimer : Tick -> Model -> Model
incrementTimer delta model =
  { model | timeSinceLastTick = model.timeSinceLastTick + delta }

incrementScore : Model -> Model
incrementScore model =
  if model.timeSinceLastTick >= 2000 then
    { model | timeSinceLastTick = 0, score = model.score + 100 }
  else
    model

view model =
  let
    lineStyle = { defaultLine |
      color = rgb 255 0 0
    , width = 1
    }
    t = Text.fromString ("Score: " ++ (toString model.score))
          |> Text.monospace
          |> Text.height 30
          |> Text.color red
  in
    t |> rightAligned
