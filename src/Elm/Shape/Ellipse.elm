module Shape.Ellipse exposing (Ellipse, draw)

import Math.Vector2 exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Svg


type alias Ellipse =
    { radius : Vec2
    }


draw : Ellipse -> List (Svg.Attribute msg) -> Svg msg
draw e attrs =
    Svg.ellipse
        ([ e.radius
            |> getX
            |> String.fromFloat
            |> Svg.rx
         , e.radius
            |> getY
            |> String.fromFloat
            |> Svg.ry
         ]
            ++ attrs
        )
        []
