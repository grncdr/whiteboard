module Whiteboard.Board exposing (..)

import Maybe exposing (andThen, withDefault)
import Debug

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Lazy exposing (lazy2)

import Whiteboard.Geometry as Geom exposing (
  Point, Rect, gridSize, points2rect)
import Whiteboard.Svg exposing (..)
import Whiteboard.Card as Card
import Whiteboard.Mouse as Mouse
import VirtualDom as Vdom
import Window.Scroll

type Direction = Horizontal | Vertical

type Msg = Mouse Mouse.Evt
         | Card Card.Model Card.Msg

type alias Model =
  { mouse : Maybe Point
  , scrollOffset : Point
  , dragStart : Maybe Point
  , cols : Int
  , rows : Int
  , cards : List Card.Model
  }

init : Int -> Int -> Model
init cols rows =
  { mouse = Nothing
  , scrollOffset = Point 0 0
  , dragStart = Nothing
  , cols = cols
  , rows = rows
  , cards = []
  }

view : Model -> Svg Msg
view {cols, rows, mouse, dragStart, cards, scrollOffset} =
  let
    bgGrid = gridLines' cols rows
    highlighted =
      case mouse of
        Nothing -> []
        Just {x, y} ->
          [ Svg.g
              [ style "stroke: orange; stroke-width: 0.5" ] 
              [ line (Point x 0) (Point x ((toFloat rows) * gridSize))
              , line (Point 0 y) (Point ((toFloat cols) * gridSize) y)
              ]
          ]

    cardView card = Vdom.map (Card card) (Card.view card)

    cardViews = (List.map cardView cards)
  in
    Svg.g [] (bgGrid :: (highlighted ++ cardViews))

gridLines cols rows =
  let
    w = (toFloat cols) * gridSize
    h = (toFloat rows) * gridSize
    background = rect [ fill "white" ] (Rect 0 0 w h)
    horizontal y =
      line (Point 0 (gridSize * (toFloat y))) (Point w (gridSize * (toFloat y)))
    vertical x =
      line (Point (gridSize * (toFloat x)) 0) (Point (gridSize * (toFloat x)) h)
    horizontals = List.map horizontal [1..rows]
    verticals = List.map vertical [1..cols]
  in
    Svg.g
      [ style "stroke: lightgrey; stroke-width: 0.5" ]
      (background :: (verticals ++ horizontals))

gridLines' = lazy2 gridLines

subscriptions model =
  let mapCardSub = \c -> Sub.map (Card c) (Card.subscriptions c)
  in Sub.batch ((Mouse.quantized Mouse) :: (List.map mapCardSub model.cards))

update msg m =
  case msg of
    Mouse mouseMsg ->
      case (mouseMsg, m.dragStart) of
        (Mouse.Move p1, Just p2) ->
          if p1 == p2
             then m
             else { m -- further handling of the drag is done by the card
                  | dragStart = Nothing
                  , cards = (Card.initFromDrag p1 p2) :: m.cards
                  }
        (Mouse.Down pt, _) -> 
          if pointHasCard pt m.cards then m else { m | dragStart = Just pt }
        (Mouse.Up pos, _) ->
          { m | dragStart = Nothing }
        _ -> m

    Card card msg ->
      { m | cards = replace card (Card.update msg) m.cards }

pointHasCard pos cards =
  case List.filter (Geom.coversPoint pos) cards of
    [] -> False
    _ -> True

-- Replaces the first occurence of elem in list with (fn elem)
replace old fn elems =
  case elems of
    [] -> []
    elem :: rest ->
      if elem == old
         then (fn elem) :: rest
         else elem :: (replace old fn rest)
