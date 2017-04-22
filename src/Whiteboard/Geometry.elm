module Whiteboard.Geometry exposing (..)

import Debug


type alias Point =
    { x : Float, y : Float }


type alias Rect =
    { x : Float, y : Float, w : Float, h : Float }


gridSize =
    8


quantize : { a | x : Float, y : Float } -> { a | x : Float, y : Float }
quantize rec =
    { rec
        | x = gridSize * (toFloat (round (rec.x / gridSize)))
        , y = gridSize * (toFloat (round (rec.y / gridSize)))
    }


coversPoint p3 { x, y, w, h } =
    (between x p3.x (x + w)) && (between y p3.y (y + h))


point x y =
    { x = x, y = y }


between x y z =
    ((x >= y) && (y >= z)) || ((x <= y) && (y <= z))


sub p1 p2 =
    { x = p1.x - p2.x, y = p1.y - p2.y }


add d pt =
    { x = d.x + pt.x, y = d.y + pt.y }


atLeast n =
    max (n * gridSize)


points2rect p1 p2 =
    let
        minX =
            min p1.x p2.x

        minY =
            min p1.y p2.y

        maxX =
            max p1.x p2.x

        maxY =
            max p1.y p2.y
    in
        { x = minX
        , y = minY
        , w = maxX - minX
        , h = maxY - minY
        }


minRect mw mh rect =
    let
        w =
            max mw rect.w

        h =
            max mh rect.h
    in
        { rect | w = w, h = h }


center : Rect -> Point
center { x, y, w, h } =
    Point (x + (w / 2)) (y + (h / 2))
