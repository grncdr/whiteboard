module Whiteboard.Cog exposing (view)

import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Whiteboard.Svg as WSvg
import Whiteboard.Svg.Path exposing (..)
import Whiteboard.Svg.Transform exposing (translate)
import Whiteboard.Geometry exposing (Rect, Point, center)


view : Rect -> List (Svg.Attribute msg) -> Svg msg
view box attrs =
  let c = center box
      size = Basics.min box.w box.h
  in g ((transform (translate box)) :: attrs)
       [ WSvg.rect [ fill "transparent" ] { box | x = 0, y = 0 }
       , drawTeeth size ]

drawTeeth size =
  let center = size / 2.0
      r1 = size / 2.5
      r2 = 0.6875 * size / 2.5
      r3 = 0.375 * size / 2.5
      toothAngle = 2.0 * pi / 7.0
      smallStep = toothAngle * 0.2
      bigStep = toothAngle * 0.6
      stepAngle = toothAngle / 4.0
      pt a r = Point (center + r * (cos a)) (center + r * (sin a))

      addTooth n path =
        path
        |> lineTo (pt (toothAngle * n - (1.2 * stepAngle)) r2)
        |> lineTo (pt (toothAngle * n - (0.4 * stepAngle)) r1)
        |> lineTo (pt (toothAngle * n + (0.4 * stepAngle)) r1)
        |> lineTo (pt (toothAngle * n + (1.2 * stepAngle)) r2)
        |> arcTo (pt (toothAngle * n + 3 * stepAngle) r2) r2 r2 0 0 1 
  in
     List.foldl
       addTooth
       (startPath (pt (0.0 - stepAngle) r2))
       (List.map toFloat [0..7])
     |> moveTo (Point center (center - r3))
     |> arcTo (Point center (center + r3)) r3 r3 0 0 0
     |> arcTo (Point center (center - r3)) r3 r3 0 0 0
     |> toSvg []
