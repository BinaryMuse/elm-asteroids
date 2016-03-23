import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time
import Window

import GameConfig exposing (GameOptions, gameOptions)
import Asteroids
import Score
import Ship

type alias Model =
  { ship: Ship.Model
  , asteroids: Asteroids.Model
  }

type Update
  = ShipUpdate Ship.Update
  | AsteroidsUpdate Asteroids.Update

model : Model
model =
  { ship = Ship.model
  , asteroids = Asteroids.generateModel 3
  }

update : Update -> Model -> Model
update update model =
  case update of
    ShipUpdate shipUpdate ->
      { model | ship = Ship.update shipUpdate model.ship }
    AsteroidsUpdate asteroidsUpdate ->
      { model | asteroids = Asteroids.update asteroidsUpdate model.asteroids }

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

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp update model input)

input : Signal Update
input =
  let
    clock = (Time.fps 30)
    signals = [
      Signal.map ShipUpdate (Ship.input clock)
    , Signal.map AsteroidsUpdate (Asteroids.input clock)
    ]
  in
    Signal.sampleOn clock (Signal.mergeMany signals)
