module Whiteboard.ColorSelector exposing (view)

import Svg exposing (Svg)
import Svg.Attributes as A
import Svg.Events as E
import Whiteboard.Svg exposing (..)
import Whiteboard.Geometry exposing (Rect)


colors =
    [ "#38ABD4"
    , "#ED6A5A"
    , "#CFDBD5"
    , "#775E66"
    , "#FFE8D1"
    ]


view : Float -> Float -> (String -> msg) -> Svg.Svg msg
view w h msg =
    let
        boxWidth =
            (w - 1) / (List.length colors |> toFloat)

        boxHeight =
            h - 1

        boxAt i color =
            rect
                [ A.fill color, E.onClick (msg color) ]
                (Rect (0.5 + boxWidth * (toFloat i)) 1 boxWidth boxHeight)

        wrapper =
            rect [ A.stroke "#666666", A.strokeWidth "0.5px" ] (Rect 0 0 w h)
    in
        Svg.g
            []
            (wrapper :: (List.indexedMap boxAt colors))
