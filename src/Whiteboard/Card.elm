module Whiteboard.Card exposing (..)

import Task
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
import Whiteboard.Backend as Backend


type Msg
    = Mouse Mouse.Evt
    | DragStart DragKind
    | SetText String
    | SetColor String
    | SetRect Rect
    | ToggleSettings Bool
    | Destroy
    | Commit Backend.Card
    | Rollback Backend.ClientError Model


type DragKind
    = Move
    | Resize


type alias Model =
    { auth : Backend.Authorization
    , boardId : String
    , id : String
    , x : Float
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


initFromBackend : Backend.Authorization -> String -> Backend.Card -> Model
initFromBackend auth boardId {id, content, x, y, w, h, color, deleted} =
  { auth = auth
  , boardId = boardId
  , id = id
  , x = x
  , y = y
  , w = w
  , h = h
  , color = color
  , textContent = content
  , mouse = { x = 0, y = 0 }
  , drag = Nothing
  , dragStart = { x = 0, y = 0 }
  , showSettings = False
  , deleted = deleted
  }

initFromDrag auth boardId p1 p2 =
    let
        { x, y, w, h } =
            points2rect p1 p2
    in
        { auth = auth
        , boardId = boardId
        , id = ""
        , x = x
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model.drag ) of
        ( Destroy, _ ) ->
          if confirm "Delete this card?" then
            saveCard model { model | showSettings = False, deleted = True } Backend.setCardDeleted
          else
            ({ model | showSettings = False }, Cmd.none)

        ( DragStart Move, _ ) ->
            ({ model | drag = Just Move, dragStart = model.mouse }, Cmd.none)

        ( DragStart Resize, _ ) ->
            ({ model | drag = Just Resize }, Cmd.none)

        ( ToggleSettings onOff, _ ) ->
            ({ model | showSettings = onOff }, Cmd.none)

        ( SetText s, _ ) ->
            let
                box = contentDimensions s
                w = max box.w model.w
                h = max box.h model.h
            in
              saveCard model { model | textContent = s, w = w, h = h } Backend.setCardContent

        ( SetColor color, _ ) ->
            saveCard model { model | color = color, showSettings = False } Backend.setCardColor

        ( SetRect { x, y, w, h }, _ ) ->
            ({ model | x = x, y = y, w = w, h = h }, Cmd.none)

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
                ({ model | w = atLeast 2 w_, h = atLeast 2 h_ }, Cmd.none)

        ( Mouse (Mouse.Move pt), Just Move ) ->
            let
                delta =
                    sub pt model.dragStart
            in
                ({ model
                    | x = model.x + delta.x
                    , y = model.y + delta.y
                    , dragStart = add model.dragStart delta
                }, Cmd.none) 
        ( Mouse (Mouse.Move pt), Nothing ) ->
            ({ model | mouse = pt }, Cmd.none)

        ( Mouse (Mouse.Up pt), Just Move ) ->
          saveCard model { model | drag = Nothing } Backend.moveCard

        ( Mouse (Mouse.Up pt), Just Resize ) ->
          saveCard model { model | drag = Nothing }
            (if model.id == "" then
               Backend.createCard model.auth.user
             else
               Backend.resizeCard)

        ( Commit data, _ ) ->
          ({ model | id = data.id }, Cmd.none)

        ( Rollback err prev, _ ) ->
          (prev, Cmd.none)

        _ ->
            (model, Cmd.none)



saveCard prev next mutationDoc =
  let cmd =
        Backend.mutate (Just prev.auth) mutationDoc next
        |> Task.attempt (\res -> case (res, prev.id) of
          (Ok data, _) -> Commit data
          (Err err, "") -> Destroy
          (Err err, _) -> Rollback err prev)
  in (next, cmd)

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
