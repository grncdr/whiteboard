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
import Whiteboard.Login as Login
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
    { auth : Maybe Backend.Authorization
    , board : Maybe Board.Model
    , allBoards : List Board.Model
    , loadError : Maybe String
    , login : Login.Model
    }


type Msg
    = Login Login.Msg
    | Board Board.Msg
    | SaveError String
    | SaveSuccess ()
    | LoadError Backend.ClientError
    | LoadSuccess (List Board.Model)



-- testing


boardid =
    "cj250lw8a153h0182zhn77uu9"


init : ( Model, Cmd Msg )
init =
    ( { login = Login.init
      , board = Nothing
      , allBoards = []
      , loadError = Nothing
      , auth = Nothing
      }
    , Cmd.none )

{-
    , Backend.query config Backend.loadBoard { boardID = boardid }
        |> Task.map (Board.initFromBackend
        |> attemptWith LoadSuccess LoadError
    )
-}


view : Model -> Html Msg
view { login, auth, board, allBoards, loadError } =
  case (auth, board) of
    (Nothing, _) -> Login.view login |> Html.map Login
    (_, Nothing) -> Html.div [] [ Html.text "loading..." ]
    (_, Just board) -> viewBoard board

viewBoard : Board.Model -> Html Msg
viewBoard board =
    let
        w =
            board.cols * gridSize |> toString

        h =
            board.rows * gridSize |> toString

        vb =
            String.join " " [ "0", "0", w, h ]

        boardHtml =
            Html.map Board (Board.view board)
    in
        svg
            [ width w
            , height h
            , viewBox vb
            ]
            [ boardHtml ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login msg ->
            let (login, cmd) = Login.update msg model.login
                loginCmd = Cmd.map Login cmd
            in case login.auth of
                Nothing ->
                    ( { model | login = login }, loginCmd )
                Just auth -> 
                    ( { model | login = login, auth = login.auth }
                    , Cmd.batch
                        [ loginCmd -- usually Cmd.none
                        , Backend.query (Just auth) Backend.loadBoards auth
                          |> Task.andThen (maybeCreateInitialBoard auth)
                          |> Task.map (List.map (Board.initFromBackend auth))
                          |> attemptWith LoadSuccess LoadError
                        ] )
                
        Board msg ->
            case (Maybe.map (Board.update msg) model.board) of
                Nothing ->
                    ( model, Cmd.none )

                Just ( board, cmd ) ->
                    ( { model | board = Just board }, Cmd.map Board cmd )

        LoadSuccess boards ->
            ( { model | allBoards = boards, board = List.head boards }
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


maybeCreateInitialBoard : Backend.Authorization -> List Backend.Board -> Task.Task Backend.ClientError (List Backend.Board)
maybeCreateInitialBoard auth boards =
    if List.length boards > 0 then Task.succeed boards
    else
        Backend.mutate (Just auth) Backend.createBoard auth
        |> Task.map (\board -> [board])


subscriptions model =
    let
        boardSubs board =
            Sub.map Board (Board.subscriptions board)
    in
        Maybe.map boardSubs model.board |> Maybe.withDefault Sub.none
