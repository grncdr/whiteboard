module Whiteboard.Menu exposing (view, link, entryHeight)

import Svg exposing (g)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (..)
import Html exposing (div, text)
import Whiteboard.Geometry exposing (Rect)
import Whiteboard.Svg exposing (rect)
import Whiteboard.Svg.Transform exposing (translate)


type alias Entry msg =
    Svg.Svg msg


entryHeight =
    20


view topLeft menuEntries =
    g [ transform (translate topLeft) ] (List.indexedMap (entryView topLeft.w) menuEntries)


entryView w idx content =
    let
        top =
            idx |> toFloat |> (*) entryHeight
    in
        g [ translate { x = 0, y = top } |> transform ]
            [ rect
                [ fill "#eeeeee"
                , strokeWidth "1px"
                , stroke "#666666"
                ]
                (Rect 0 0 w entryHeight)
            , content
            ]


link label msg =
    Svg.text_
        [ onClick msg
        , class "clickable"
        , fontSize "12px"
        , fontFamily "monospace"
        , y "14"
        , x "4"
        ]
        [ Svg.text label ]
