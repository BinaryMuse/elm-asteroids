module Main where

import Graphics.Element exposing (..)
import Window

import Game exposing (model, update, view, input)

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update model input)
