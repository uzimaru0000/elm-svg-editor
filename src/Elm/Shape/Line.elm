module Shape.Line exposing (Line, draw)

import Math.Vector2 exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Svg


type alias Line =
    { start : Vec2
    , end : Vec2
    }


draw : Line -> List (Svg.Attribute msg) -> Svg msg
draw line attrs =
    Svg.line
        ([ line.start
            |> getX
            |> String.fromFloat
            |> Svg.x1
         , line.start
            |> getY
            |> String.fromFloat
            |> Svg.y1
         , line.end
            |> getX
            |> String.fromFloat
            |> Svg.x2
         , line.end
            |> getY
            |> String.fromFloat
            |> Svg.y2
         ]
            ++ attrs
        )
        []
