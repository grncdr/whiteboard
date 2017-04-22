module Whiteboard.Card exposing (..)

import String
import Svg exposing (Svg)
import Svg.Events exposing (onMouseDown, onMouseUp)
import Svg.Attributes exposing (fill, stroke, strokeWidth, transform)
import Html exposing (textarea, text)
import Html.Attributes
import Html.Events exposing (onInput)
import Confirm exposing (confirm)
import Whiteboard.Svg exposing (..)
import Whiteboard.Svg.Transform exposing (translate)
import Whiteboard.Geometry exposing (Point, Rect, add, sub, quantize, gridSize, atLeast, points2rect)
import Whiteboard.Mouse as Mouse
import Whiteboard.Cog as Cog
import Whiteboard.Menu as Menu
import Whiteboard.ColorSelector as ColorSelector


type Msg
    = Mouse Mouse.Evt
    | DragStart DragKind
    | SetText String
    | SetColor String
    | SetRect Rect
    | ToggleSettings Bool
    | Destroy


type DragKind
    = Move
    | Resize


type alias Model =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , color : String
    , textContent : String
    , mouse : Point
    , drag : Maybe DragKind
    , dragStart : Point
    , showSettings : Bool
    , deleted : Bool
    }


initFromDrag p1 p2 =
    let
        { x, y, w, h } =
            points2rect p1 p2
    in
        { x = x
        , y = y
        , w = w
        , h = h
        , color = "white"
        , mouse = p2
        , dragStart = p2
        , drag = Just Resize
        , showSettings = False
        , textContent = ""
        , deleted = False
        }


view : Model -> Svg Msg
view { x, y, w, h, color, mouse, drag, showSettings, textContent } =
    let
        topLeft =
            (Point x y)

        card =
            rect
                [ strokeWidth "1"
                , stroke "black"
                , fill color
                , onMouseDown (DragStart Move)
                ]
                (Rect 0 0 w h)
                |> Just

        borderSize =
            min (min w h) (gridSize * 2)

        resizer =
            Just <|
                triangle
                    [ fill "#888"
                    , Svg.Attributes.class "resizer"
                    , onMouseDown (DragStart Resize)
                    , transform
                        (translate
                            { x = w - borderSize
                            , y = h - borderSize
                            }
                        )
                    ]
                    (Point 0 (borderSize - 1))
                    (Point (borderSize - 1) 0)
                    (Point (borderSize - 1) (borderSize - 1))

        cog =
            Cog.view
                { x = 0, y = h - borderSize, w = borderSize, h = borderSize }
                [ onMouseDown (ToggleSettings (not showSettings))
                , Svg.Attributes.class "clickable"
                ]
                |> Just

        settings =
            if showSettings then
                Just <|
                    Menu.view
                        { x = 0, y = h, w = w }
                        [ ColorSelector.view w Menu.entryHeight SetColor
                        , Menu.link "Destroy!" Destroy
                        ]
            else
                Nothing

        textbox =
            if (min w h) < (gridSize * 4) then
                Nothing
            else
                Just <|
                    Svg.foreignObject
                        [ Svg.Attributes.width (toString w)
                        , Svg.Attributes.height (toString h)
                        ]
                        [ textarea
                            [ Html.Attributes.style
                                [ ( "width", px w )
                                , ( "height", px (h - (2 * gridSize)) )
                                ]
                            , Html.Attributes.class "card-textarea"
                            , onInput SetText
                            ]
                            [ Html.text textContent ]
                        ]
    in
        Svg.g
            [ transform ("translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")") ]
            (List.filterMap identity [ card, cog, resizer, textbox, settings ])


px i =
    (toString i) ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.quantized Mouse


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.drag ) of
        ( Destroy, _ ) ->
            { model | deleted = confirm "Delete this card?", showSettings = False }

        ( DragStart Move, _ ) ->
            { model | drag = Just Move, dragStart = model.mouse }

        ( DragStart Resize, _ ) ->
            { model | drag = Just Resize }

        ( ToggleSettings onOff, _ ) ->
            { model | showSettings = onOff }

        ( SetText s, _ ) ->
            let
                content =
                    contentDimensions s

                w_ =
                    max content.w model.w

                h_ =
                    max content.h model.h
            in
                { model | textContent = s, w = w_, h = h_ }

        ( SetColor color, _ ) ->
            { model | color = color, showSettings = False }

        ( SetRect { x, y, w, h }, _ ) ->
            { model | x = x, y = y, w = w, h = h }

        ( Mouse (Mouse.Move pt), Just Resize ) ->
            let
                dragW =
                    pt.x - model.x

                dragH =
                    pt.y - model.y

                content =
                    contentDimensions model.textContent

                w_ =
                    max dragW content.w

                h_ =
                    max dragH content.h
            in
                { model | w = atLeast 2 w_, h = atLeast 2 h_ }

        ( Mouse (Mouse.Move pt), Just Move ) ->
            let
                delta =
                    sub pt model.dragStart
            in
                { model
                    | x = model.x + delta.x
                    , y = model.y + delta.y
                    , dragStart = add model.dragStart delta
                }

        ( Mouse (Mouse.Move pt), Nothing ) ->
            { model | mouse = pt }

        ( Mouse (Mouse.Up pt), Just _ ) ->
            { model | drag = Nothing }

        _ ->
            model


contentDimensions s =
    let
        reducer line ( count, maxLen ) =
            ( count + 1
            , if String.length line > maxLen then
                String.length line
              else
                maxLen
            )

        normalize =
            ceiling >> toFloat >> (*) gridSize

        toPx c r n =
            n |> toFloat |> (*) r |> (+) c |> normalize
    in
        String.split "\n" s
            |> List.foldl reducer ( 0, 0 )
            |> \( rows, cols ) ->
                { w = toPx 0.5 0.91 cols
                , h = toPx 2.1 1.51 rows
                }
