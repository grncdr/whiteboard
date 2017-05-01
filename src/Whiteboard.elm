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

import Whiteboard.Backend as Backend
import Whiteboard.Board as Board
import Whiteboard.Geometry exposing (gridSize)
import Util exposing (attemptWith)
import Debug


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Maybe Board.Model
    , loadError : Maybe String
    }


type Msg
    = Board Board.Msg
    | SaveError String
    | SaveSuccess ()
    | LoadError Backend.ClientError
    | LoadSuccess Board.Model



-- testing


boardid =
    "cj250lw8a153h0182zhn77uu9"


init : ( Model, Cmd Msg )
init =
    ( { board = Nothing, loadError = Nothing }
    , Backend.query Backend.loadBoard { boardID = boardid }
        |> Task.map Board.fromBackend
        |> attemptWith LoadSuccess LoadError
    )


view : Model -> Html Msg
view { board, loadError } =
  case board of
    Just b -> viewBoard b
    Nothing -> Html.p [] [ Html.text (Maybe.withDefault "Loading..." loadError) ]


viewBoard : Board.Model -> Html Msg
viewBoard board =
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
            case (Maybe.map (Board.update msg) model.board) of
                Nothing ->
                    ( model, Cmd.none )

                Just ( board, cmd ) ->
                    ( { model | board = Just board }, Cmd.map Board cmd )

        LoadSuccess board ->
            ( { model | board = Just board }
            , Time.now
                |> Task.perform
                    (round >> Random.initialSeed >> Board.SetSeed >> Board)
            )

        LoadError err ->
            ( { model | loadError = Just (toString err) }, Cmd.none )

        SaveSuccess _ ->
            ( model, Cmd.none )

        SaveError err ->
            let
                _ =
                    Debug.log "save error" err
            in
                ( model, Cmd.none )


subscriptions model =
    let
        boardSubs board =
            Sub.map Board (Board.subscriptions board)
    in
        Maybe.map boardSubs model.board |> Maybe.withDefault Sub.none
