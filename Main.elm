import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time
import Window

import GameConfig exposing (GameOptions, gameOptions)
import Ship

type alias Model =
  { ship: Ship.Model }

type Update
  = ShipUpdate Ship.Update

model : Model
model =
  { ship = Ship.model }

update : Update -> Model -> Model
update update model =
  case update of
    ShipUpdate shipUpdate ->
      { model | ship = Ship.update shipUpdate model.ship }

view : (Int, Int) -> Model -> Element
view (w, h) model =
  collage w h [
    toForm <| Ship.view (w, h) model.ship
  ]

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update model input)

input : Signal Update
input =
  let
    clock = (Time.fps 30)
    signals = [
      Signal.map ShipUpdate (Ship.input clock)
    ]
  in
    Signal.sampleOn clock (Signal.mergeMany signals)
