module Shape.Rect exposing (Rect, create, draw)

import Math.Vector2 exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Svg


type alias Rect =
    { size : Vec2
    , radius : Vec2
    }


create : Vec2 -> Vec2 -> Rect
create start end =
    let
        w =
            getX end
                - getX start
                |> abs

        h =
            getY end
                - getY start
                |> abs
    in
    { size = vec2 w h
    , radius = vec2 0 0
    }


draw : Rect -> List (Svg.Attribute msg) -> Svg msg
draw r attrs =
    Svg.rect
        ([ getX r.size
            |> String.fromFloat
            |> Svg.width
         , getY r.size
            |> String.fromFloat
            |> Svg.height
         , getX r.size
            |> (*) -0.5
            |> String.fromFloat
            |> Svg.x
         , getY r.size
            |> (*) -0.5
            |> String.fromFloat
            |> Svg.y
         , getX r.radius
            |> String.fromFloat
            |> Svg.rx
         , getY r.radius
            |> String.fromFloat
            |> Svg.ry
         ]
            ++ attrs
        )
        []
