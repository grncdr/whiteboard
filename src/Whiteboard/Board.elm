module Whiteboard.Board exposing (..)

import Char
import Debug
import Dict exposing (Dict)
import Maybe exposing (andThen, withDefault)
import Random
import String
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy2)
import Whiteboard.Geometry as Geom
    exposing
        ( Point
        , Rect
        , gridSize
        , points2rect
        )
import Whiteboard.Svg exposing (..)
import Whiteboard.Backend as Backend
import Whiteboard.Card as Card
import Whiteboard.Mouse as Mouse
import VirtualDom as Vdom


type Direction
    = Horizontal
    | Vertical


type Msg
    = Mouse Mouse.Evt
    | AddCard Id Card.Model
    | AlterCard String Card.Msg
    | RemoveCard Id
    | SetSeed Random.Seed


type alias Id =
    String


type alias Model =
    { seed : Random.Seed
    , auth : Backend.Authorization
    , boardId : Id
    , mouse : Maybe Point
    , scrollOffset : Point
    , dragStart : Maybe Point
    , cols : Int
    , rows : Int
    , cards : Dict Id Card.Model
    }


initFromBackend : Backend.Authorization -> Backend.Board -> Model
initFromBackend auth { id, w, h, cards } =
    let
        insertCard backendCard =
            Dict.insert backendCard.id (Card.initFromBackend auth id backendCard)
    in
        { auth = auth
        , boardId = id
        , seed = Random.initialSeed 0
        , mouse = Nothing
        , scrollOffset = Point 0 0
        , dragStart = Nothing
        , cols = w
        , rows = h
        , cards = List.foldl insertCard Dict.empty cards
        }


view : Model -> Svg Msg
view { cols, rows, mouse, dragStart, cards, scrollOffset } =
    let
        bgGrid =
            gridLines_ cols rows

        highlighted =
            case mouse of
                Nothing ->
                    []

                Just { x, y } ->
                    [ Svg.g
                        [ style "stroke: orange; stroke-width: 0.5" ]
                        [ line (Point x 0) (Point x ((toFloat rows) * gridSize))
                        , line (Point 0 y) (Point ((toFloat cols) * gridSize) y)
                        ]
                    ]

        cardView ( id, card ) =
            if card.deleted then
                Nothing
            else
                Card.view card |> Vdom.map (AlterCard id) |> Just

        cardViews =
            Dict.toList cards |> List.filterMap cardView
    in
        Svg.g [] (bgGrid :: (highlighted ++ cardViews))


gridLines cols rows =
    let
        w =
            (toFloat cols) * gridSize

        h =
            (toFloat rows) * gridSize

        background =
            rect [ fill "white" ] (Rect 0 0 w h)

        horizontal y =
            line (Point 0 (gridSize * (toFloat y))) (Point w (gridSize * (toFloat y)))

        vertical x =
            line (Point (gridSize * (toFloat x)) 0) (Point (gridSize * (toFloat x)) h)

        horizontals =
            List.map horizontal (List.range 1 rows)

        verticals =
            List.map vertical (List.range 1 cols)
    in
        Svg.g
            [ style "stroke: lightgrey; stroke-width: 0.5" ]
            (background :: (verticals ++ horizontals))


gridLines_ =
    lazy2 gridLines


subscriptions model =
    let
        mapCardSub ( id, card ) =
            Sub.map (AlterCard id) (Card.subscriptions card)

        mouse =
            Mouse.quantized Mouse

        cardSubs =
            Dict.toList model.cards |> List.map mapCardSub
    in
        Sub.batch (mouse :: cardSubs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Mouse mouseMsg ->
            mouseUpdate mouseMsg m

        SetSeed seed ->
            ( { m | seed = seed }, Cmd.none )

        -- Used by log replay only, normally cards are added by mouse interaction
        AddCard id card ->
            ( { m | cards = Dict.insert id card m.cards }, Cmd.none )

        AlterCard id msg ->
          case (Dict.get id m.cards |> Maybe.map (Card.update msg)) of
            Nothing ->
              (m, Cmd.none)
            Just (newCard, cmd) ->
              ( { m | cards = Dict.insert id newCard m.cards }, Cmd.map (AlterCard id) cmd)

        -- Same as above, used only by log replay
        RemoveCard id ->
            ( { m | cards = Dict.update id (Maybe.map (\c -> { c | deleted = True })) m.cards }, Cmd.none )


mouseUpdate : Mouse.Evt -> Model -> (Model, Cmd Msg)
mouseUpdate mouseMsg m =
    case ( mouseMsg, m.dragStart ) of
        ( Mouse.Move p1, Just p2 ) ->
            if p1 == p2 then
                (m, Cmd.none)
            else
                let
                    ( id, seed ) =
                        Random.step cardIdGenerator m.seed
                in
                    ( { m
                        | seed = seed
                        , cards = Dict.insert "" (Card.initFromDrag m.auth m.boardId p1 p2) m.cards
                        , dragStart = Nothing -- further handling of the drag is done by the card
                      }
                    , Cmd.none
                    )

        ( Mouse.Down pt, _ ) ->
            if pointHasCard pt m.cards then
                (m, Cmd.none)
            else
                ( { m | dragStart = Just pt }, Cmd.none )

        ( Mouse.Up pos, _ ) ->
            ( { m | dragStart = Nothing }, Cmd.none )

        _ ->
            ( m, Cmd.none )


pointHasCard pos cards =
    Dict.filter (\id card -> Geom.coversPoint pos card) cards
        |> Dict.isEmpty
        |> not



-- Replaces the first item that passes `pred` with `fn item`


replace pred fn elems =
    case elems of
        [] ->
            []

        elem :: rest ->
            if pred elem then
                (fn elem) :: rest
            else
                elem :: (replace pred fn rest)


cardIdGenerator =
    let
        i2s i =
            Char.fromCode
                (i
                    + (if i < 10 then
                        48
                       else
                        55
                      )
                )
                |> String.fromChar
    in
        Random.int 0 36
            |> Random.map i2s
            |> Random.list 12
            |> Random.map (String.join "")
