module Whiteboard.Svg.Transform exposing (..)

translate pt =
  "translate(" ++ (toString pt.x) ++ " " ++ (toString pt.y) ++ ")" 
