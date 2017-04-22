port module Whiteboard.Mouse exposing (Evt(..), mouse, quantized)

import Debug
import Whiteboard.Geometry exposing (Point, quantize)


type Evt
    = Move Point
    | Down Point
    | Up Point



-- mouse events with absolute document coordinates


mouse : (Evt -> msg) -> Sub msg
mouse f =
    rawmouse (toEvent >> f)



-- mouse events with absolute document coordinates quantized to the grid size


quantized : (Evt -> msg) -> Sub msg
quantized f =
    rawmouse (quantize >> toEvent >> f)



-- Module internals for converting from raw records to Evt type union


type alias RawEvt =
    { kind : String
    , x : Float
    , y : Float
    }


port rawmouse : (RawEvt -> msg) -> Sub msg


toEvent : RawEvt -> Evt
toEvent { kind, x, y } =
    let
        ctor =
            case kind of
                "up" ->
                    Up

                "down" ->
                    Down

                "move" ->
                    Move

                _ ->
                    Debug.crash "Bad message on mouse port"
    in
        Point x y |> ctor
