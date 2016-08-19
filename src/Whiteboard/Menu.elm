module Whiteboard.Menu exposing (view)

import Svg exposing (g)
import Svg.Attributes exposing (..)

import Html exposing (div, text)

import Whiteboard.Geometry exposing (Rect)
import Whiteboard.Svg exposing (rect)
import Whiteboard.Svg.Transform exposing (translate)

type alias Entry msg =
  { label : String
  , onSelect : Maybe msg
  }

entryHeight = 16

view topLeft menuEntries =
  g [ transform (translate topLeft) ] (List.indexedMap entryView menuEntries)

entryView idx {label} =
  let top = idx |> toFloat |> (*) entryHeight
  in g [ translate { x = 0, y = top } |> transform ]
       [ rect
          [ fill "#888888" ]
          (Rect 0 0 (entryHeight * 4) entryHeight)
       , Svg.text'
           [ fontSize "12px"
           , fontFamily "monospace"
           , y "12"
           ]
           [ Svg.text label ]
       ]
