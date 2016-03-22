module Score where

import Graphics.Collage exposing (defaultLine, outlinedText, toForm)
import Graphics.Element exposing (rightAligned)
import Text exposing (fromString)

import Colors exposing (..)

view =
  let
    lineStyle = { defaultLine |
      color = rgb 255 0 0
    , width = 1
    }
    t = Text.fromString "Score: 0"
          |> Text.monospace
          |> Text.height 30
          |> Text.color red
  in
    t |> rightAligned
