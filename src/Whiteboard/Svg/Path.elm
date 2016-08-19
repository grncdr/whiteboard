module Whiteboard.Svg.Path exposing (startPath, moveTo, lineTo, arcTo, toSvg)

import String exposing (join)
import Svg
import Svg.Attributes

import Whiteboard.Geometry exposing (Point)

type PathCommand = Move Point
                 | Line Point
                 | Arc Float Float Int Int Int Point

startPath pt = [Move pt]
moveTo pt path = (Move pt) :: path
lineTo pt path = (Line pt) :: path
arcTo p rx ry rotX large sweep path = (Arc rx ry rotX large sweep p) :: path

toSvg : List (Svg.Attribute msg) -> List PathCommand -> Svg.Svg msg
toSvg attrs cmds =
  Svg.path ((Svg.Attributes.d (commandsToString cmds)) :: attrs) []

commandsToString cmds =
  List.reverse cmds |> List.map commandToString |> join " "

commandToString cmd =
  join " " <| case cmd of
    Move p -> ["M", toString p.x, toString p.y]
    Line p -> ["L", toString p.x, toString p.y]
    Arc rx ry rotX large sweep p ->
      [ "A"
      , toString rx
      , toString ry
      , toString rotX
      , toString large
      , toString sweep
      , toString p.x
      , toString p.y
      ]

