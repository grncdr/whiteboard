module Whiteboard.Card exposing (..)

import String

import Svg exposing (Svg)
import Svg.Events exposing (onMouseDown, onMouseUp)
import Svg.Attributes exposing (fill, stroke, strokeWidth, transform)

import Html exposing (textarea, text)
import Html.Attributes
import Html.Events exposing (onInput)

import Whiteboard.Svg exposing (..)
import Whiteboard.Svg.Transform exposing (translate)
import Whiteboard.Geometry exposing (Point, Rect, add, sub, quantize, gridSize, atLeast, points2rect)
import Whiteboard.Mouse as Mouse

import Whiteboard.Cog as Cog
import Whiteboard.Menu as Menu

type Msg = Mouse Mouse.Evt
         | DragStart DragKind
         | SetText String
         | ToggleSettings Bool

type DragKind = Move | Resize

type alias Model =
  { x : Float
  , y : Float
  , w : Float
  , h : Float
  , border : String
  , mouse : Point
  , drag : Maybe DragKind
  , dragStart : Point
  , showSettings : Bool
  , textContent : String
  }

initFromDrag p1 p2 =
  let {x,y,w,h} = points2rect p1 p2
  in { x = x
     , y = y
     , w = w
     , h = h
     , border = "black"
     , mouse = p2
     , dragStart = p2
     , drag = Just Resize
     , showSettings = False
     , textContent = ""
     }

view : Model -> Svg Msg
view { x, y, w, h, border, mouse, drag, showSettings, textContent } =
  let
    (x', y', w', h') = (x, y, w, h)

    topLeft = (Point x' y')

    card =
      rect
        [ strokeWidth "1"
        , stroke border
        , fill "white"
        , onMouseDown (DragStart Move)
        ]
        (Rect 0 0 w' h')
      |> Just

    borderSize = min (min w' h') (gridSize * 2)
    resizer =
      Just <| triangle
        [ fill "#888"
        , onMouseDown (DragStart Resize)
        , transform (translate { x = w' - borderSize
                               , y = h' - borderSize })
        ]
        (Point 0 (borderSize - 1))
        (Point (borderSize - 1) 0)
        (Point (borderSize - 1) (borderSize - 1))

    cog =
      Cog.view
        { x = 0, y = h' - borderSize, w = borderSize, h = borderSize }
        [ onMouseDown (ToggleSettings (not showSettings)) ]
      |> Just

    settings =
      if showSettings
         then Just <| Menu.view { x = 0, y = h' }
                                [ { label = "First Option" }
                                , { label = "Second Option" }
                                ]
         else Nothing

    textbox =
      if (min w' h') < (gridSize * 4) then Nothing else
         Just <| Svg.foreignObject
          [ Svg.Attributes.width (toString w')
          , Svg.Attributes.height (toString h')
          ]
          [ textarea
              [ Html.Attributes.style
                  [ ("width", px w'), ("height", px (h' - (2 * gridSize))) ]
              , Html.Attributes.class "card-textarea"
              , onInput SetText
              ]
              [ text textContent ]
          ]
  in
    Svg.g
      [ transform ("translate(" ++ (toString x') ++ "," ++ (toString y') ++ ")") ]
      (List.filterMap identity [ card, cog, resizer, textbox, settings ])

px i = (toString i) ++ "px"

subscriptions : Model -> Sub Msg
subscriptions model =
  Mouse.quantized Mouse

update : Msg -> Model -> Model
update msg model =
  case (msg, model.drag) of
    (DragStart Move, _) ->
      { model | drag = Just Move, dragStart = model.mouse }
    (DragStart Resize, _) ->
      { model | drag = Just Resize }
    (ToggleSettings onOff, _) ->
      let _ = Debug.log "ToggleSettings" onOff
      in { model | showSettings = onOff }
    (SetText s, _) ->
      let content = contentDimensions s
          w' = max content.w model.w
          h' = max content.h model.h
      in { model | textContent = s, w = w', h = h' }
    (Mouse (Mouse.Move pt), Just Resize) ->
      let dragW = pt.x - model.x
          dragH = pt.y - model.y
          content = contentDimensions model.textContent
          w' = max dragW content.w
          h' = max dragH content.h
      in { model | w = atLeast 2 w' , h = atLeast 2 h' }
    (Mouse (Mouse.Move pt), Just Move) ->
      let delta = sub pt model.dragStart
      in { model
         | x = model.x + delta.x
         , y = model.y + delta.y
         , dragStart = add model.dragStart delta
         }
    (Mouse (Mouse.Move pt), Nothing) ->
      { model | mouse = pt }
    (Mouse (Mouse.Up pt), Just _) ->
      { model | drag = Nothing }
    _ -> model

contentDimensions s =
  let reducer line (count, maxLen) =
        ( count + 1
        , if String.length line > maxLen then String.length line else maxLen
        )
      normalize = ceiling >> toFloat >> (*) gridSize
      toPx c r n = n |> toFloat |> (*) r |> (+) c |> normalize
  in
  String.split "\n" s
  |> List.foldl reducer (0, 0)
  |> \(rows, cols) ->
        { w = toPx 0.5 0.91 cols
        , h = toPx 2.1 1.51 rows
        }
