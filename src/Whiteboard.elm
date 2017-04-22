module Whiteboard exposing (..)

import Json.Encode as E
import Json.Decode as D
import Random
import String
import Task
import Time
import Html exposing (Html)
import Svg exposing (svg)
import Svg.Attributes exposing (..)
--import Whiteboard.Persist as Persist
import Whiteboard.Board as Board
import Whiteboard.Geometry exposing (gridSize)
import Debug


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board.Model }


type Msg
    = Board Board.Msg
    | SaveError String
    | SaveSuccess ()
    | LoadError String
    | LoadSuccess Board.Model


init : ( Model, Cmd Msg )
init =
    ( { board = Board.init 256 128 }
    , Task.perform LoadSuccess (Task.succeed (Board.init 256 128))
    )


view : Model -> Html Msg
view { board } =
    let
        w =
            board.cols * gridSize |> toString

        h =
            board.rows * gridSize |> toString

        vb =
            String.join " " [ "0", "0", w, h ]

        boardView =
            Html.map Board (Board.view board)
    in
        svg
            [ width w
            , height h
            , viewBox vb
            ]
            [ boardView ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Board msg ->
            let
                board = Board.update msg model.board
            in
                ( { model | board = board }
                , Cmd.none
                )

        LoadSuccess board ->
            ( { model | board = board }
            , Time.now
                |> Task.perform
                    (round >> Random.initialSeed >> Board.SetSeed >> Board)
            )

        LoadError err ->
            let
                _ =
                    Debug.log "load error" err
            in
                ( model, Cmd.none )

        SaveSuccess _ ->
            ( model, Cmd.none )

        SaveError err ->
            let
                _ =
                    Debug.log "save error" err
            in
                ( model, Cmd.none )


subscriptions model =
    Sub.batch [ Sub.map Board (Board.subscriptions model.board) ]
