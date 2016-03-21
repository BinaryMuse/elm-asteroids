import Keyboard
import Time exposing (..)
import Window
import Graphics.Element exposing (..)

import GameConfig exposing (GameOptions, gameOptions)
import Ship

main : Signal Element
main = Signal.map2 Ship.view Window.dimensions (Signal.foldp Ship.update Ship.model input)

-- `input` is a Signal ...
input : Signal (Float, Ship.Keys, Bool)
input =
  let
    -- ... that updates every 33 ms ....
    delta = (fps 30)
  in
    -- ... that provides a tuple of the time since last update and the Keyboard info
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.space)
