module Whiteboard exposing (..)

import String

import Html exposing (Html)
import Html.App
import Svg exposing (svg)
import Svg.Attributes exposing (..)

import Whiteboard.Board as Board
import Whiteboard.Geometry exposing (gridSize)

import LocalStorage

main =
  Html.App.program
    { init = init 256 128
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { grid : Board.Model }

type Msg = Board Board.Msg

init : Int -> Int -> (Model, Cmd Msg)
init cols rows =
  ( { grid = Board.init cols rows }
  , Cmd.none)

view : Model -> Html Msg
view {grid} = 
  let w = grid.cols * gridSize |> toString
      h = grid.rows * gridSize |> toString
      vb = String.join " " ["0", "0", w, h]
      gridView = Html.App.map Board (Board.view grid)
  in
    svg
      [ width w
      , height h
      , viewBox vb ]
      [ gridView ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Board msg -> ({ model | grid = Board.update msg model.grid }, Cmd.none)

subscriptions model =
  Sub.batch [Sub.map Board (Board.subscriptions model.grid)]
