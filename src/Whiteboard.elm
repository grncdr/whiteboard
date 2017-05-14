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
import RouteUrl
import Navigation
import Debug


main =
    RouteUrl.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , delta2url = delta2url
        , location2messages = location2messages
        }


type Page
    = LoginPage (Maybe Page)
    | BoardPage String
    | BoardsPage


type alias Model =
    { auth : Maybe Backend.Authorization
    , page : Page
    , board : Maybe Board.Model
    , allBoards : List Board.Model
    , loadError : Maybe String
    , login : Login.Model
    }


type Msg
    = Login Login.Msg
    | Board Board.Msg
    | LoadError Backend.ClientError
    | LoadSuccess (List Backend.Board)
    | Goto Page


init : ( Model, Cmd Msg )
init =
    ( { login = Login.init
      , board = Nothing
      , allBoards = []
      , loadError = Nothing
      , auth = Nothing
      , page = LoginPage Nothing
      }
    , Cmd.none
    )


delta2url : Model -> Model -> Maybe RouteUrl.UrlChange
delta2url old new =
    case ( old.page, new.page ) of
        ( _, LoginPage newUrl ) ->
            Just { entry = RouteUrl.ModifyEntry, url = pageUrl new.page }

        ( prev, next ) ->
            if prev == next then
                Nothing
            else
                Just { entry = RouteUrl.NewEntry, url = pageUrl next }


location2messages : Navigation.Location -> List Msg
location2messages { pathname, search } =
    let
        segments =
            String.split "/" pathname |> List.tail
    in
        case segments of
            Just [ "login" ] ->
                -- TODO: next page query param
                [ Goto <| LoginPage Nothing ]

            Just [ "boards", id ] ->
                [ Goto (BoardPage id) ]

            _ ->
                [ Goto BoardsPage ]


pageUrl page =
    case page of
        LoginPage next ->
            "/login"
                ++ (case next of
                        Just nextPage ->
                            "?next=" ++ (pageUrl nextPage)

                        Nothing ->
                            ""
                   )

        BoardsPage ->
            "/boards"

        BoardPage id ->
            "/boards/" ++ id


goto page =
    Task.perform Goto (Task.succeed page)


nextPage page =
    case page of
        LoginPage Nothing ->
            BoardsPage

        LoginPage (Just next) ->
            next

        _ ->
            page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Goto dest ->
            case ( dest, model.auth, model.allBoards ) of
                ( LoginPage _, _, _ ) ->
                    ( { model | page = dest }, Cmd.none )

                ( nextPage, Nothing, _ ) ->
                    ( model, nextPage |> Just |> LoginPage |> goto )

                ( _, Just auth, [] ) ->
                    -- we're authorized but have no boards
                    -- load everything here?
                    ( model
                    , Backend.query model.auth Backend.loadBoards auth
                        |> Task.andThen (maybeCreateInitialBoard auth)
                        |> attemptWith LoadSuccess LoadError
                    )

                ( BoardsPage, auth, boards ) ->
                    -- psych! this page doesn't exist. Go to the first board
                    case List.head boards of
                        Nothing ->
                            Debug.crash "Non-empty list was empty?"

                        Just board ->
                            ( { model | board = Just board, page = BoardPage board.boardId }
                            , Cmd.none
                            )

                ( BoardPage id, auth, boards ) ->
                    -- we're authed and boards are loaded, try to show the requested board
                    let
                        board =
                            boards
                                |> List.filter (\board -> board.boardId == id)
                                |> List.head
                    in
                        case board of
                            Nothing ->
                                ( model, goto BoardsPage )

                            Just _ ->
                                ( { model | board = board, page = dest }
                                , Cmd.none
                                )

        Login msg ->
            let
                ( login, cmd ) =
                    Login.update msg model.login

                loginCmd =
                    Cmd.map Login cmd
            in
                case login.auth of
                    Nothing ->
                        ( { model | login = login }, loginCmd )

                    Just auth ->
                        ( { model | login = login, auth = login.auth }
                        , Cmd.batch [ loginCmd, goto (nextPage model.page) ]
                        )

        LoadError err ->
            ( { model | loadError = Just (toString err) }, Cmd.none )

        LoadSuccess data ->
            case model.auth of
                Nothing ->
                    Debug.crash "LoadSuccess with auth = Nothing"

                Just auth ->
                    let
                        boards =
                            List.map (Board.initFromBackend auth) data
                    in
                        ( { model | allBoards = boards, board = List.head boards }
                        , Cmd.batch
                            [ Time.now |> Task.perform (round >> Random.initialSeed >> Board.SetSeed >> Board)
                            , goto (nextPage model.page)
                            ]
                        )

        Board msg ->
            case (Maybe.map (Board.update msg) model.board) of
                Nothing ->
                    ( model, Cmd.none )

                Just ( board, cmd ) ->
                    ( { model | board = Just board }, Cmd.map Board cmd )


maybeCreateInitialBoard : Backend.Authorization -> List Backend.Board -> Task.Task Backend.ClientError (List Backend.Board)
maybeCreateInitialBoard auth boards =
    if List.length boards > 0 then
        Task.succeed boards
    else
        Backend.mutate (Just auth) Backend.createBoard auth
            |> Task.andThen (\board -> 
                Backend.mutate (Just auth) (Backend.createCard auth.user)
                  { boardId = board.id 
                  , x = 24.0
                  , y = 24.0
                  , textContent =
                      "Welcome to the whiteboard\n" ++
                      "=========================\n\n" ++
                      "Add a card by dragging on the background.\n\n" ++
                      "Resize a card by dragging the bottom-\n" ++
                      "right corner.\n\n" ++
                      "Move a card by dragging the bottom bar.\n\n" ++
                      "Delete a card with the cog icon.\n\n" ++
                      "Have fun."
                  , w = 304.0
                  , h = 264.0
                  , color = "white"
                  }
                |> Task.map (\card -> [ { board | cards = [card] } ])
            )


subscriptions model =
    let
        boardSubs board =
            Sub.map Board (Board.subscriptions board)
    in
        Maybe.map boardSubs model.board |> Maybe.withDefault Sub.none


view : Model -> Html Msg
view model =
    case ( model.page, model.board ) of
        ( LoginPage _, _ ) ->
            Login.view model.login |> Html.map Login

        ( _, Nothing ) ->
            Html.p [] [ Html.text "Loading..." ]

        ( _, Just board ) ->
            viewBoard board


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
