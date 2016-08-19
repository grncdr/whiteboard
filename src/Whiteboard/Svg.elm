module Whiteboard.Svg exposing (..)

import String
import Svg
import Svg.Attributes as A
import Whiteboard.Geometry exposing (Point, Rect)


type alias Attrs msg = List (Svg.Attribute msg)

line : Point -> Point -> Svg.Svg msg
line p1 p2 =
  Svg.line
    [ A.x1 (toString p1.x)
    , A.y1 (toString p1.y)
    , A.x2 (toString p2.x)
    , A.y2 (toString p2.y)
    ]
    []

rect : Attrs msg -> Rect -> Svg.Svg msg
rect attrs {x, y, w, h} =
  let attrs' = [ A.x (toString x)
               , A.y (toString y)
               , A.width (toString w)
               , A.height (toString h)
               ]
  in Svg.rect (attrs ++ attrs') []

triangle : Attrs msg -> Point -> Point -> Point -> Svg.Svg msg
triangle attrs p1 p2 p3 =
  let points = [p1, p2, p3]
    |> List.map (\p -> (toString p.x) ++ "," ++ (toString p.y))
    |> String.join " "
  in Svg.polygon ((A.points points) :: attrs) []
