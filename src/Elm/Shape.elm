module Shape exposing (Modifire, Shape(..), draw, mat4, modifire, transform, transformToString)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Shape.Ellipse as Ellipse exposing (Ellipse)
import Shape.Line as Line exposing (Line)
import Shape.Rect as Rect exposing (Rect)
import Svg exposing (Svg)
import Svg.Attributes as Svg


type Shape
    = Rect Rect Modifire
    | Ellipse Ellipse Modifire
    | Line Line Modifire
    | Group (List Shape) Modifire


type alias Modifire =
    { stroke : Maybe String
    , fill : Maybe String
    , translate : Vec2
    , rotate : Float
    , scale : Vec2
    }


modifire : Modifire
modifire =
    { stroke = Nothing
    , fill = Nothing
    , translate = Vec2.vec2 0 0
    , rotate = 0.0
    , scale = Vec2.vec2 1 1
    }


draw : List (Svg.Attribute msg) -> Shape -> Svg msg
draw attrs shape =
    let
        drawOptions mod =
            [ transform mod
                |> transformToString
                |> Svg.transform
            , Maybe.withDefault "" mod.fill |> Svg.fill
            , Maybe.withDefault "" mod.stroke |> Svg.stroke
            ]
    in
    case shape of
        Rect rect mod ->
            Rect.draw
                rect
                (drawOptions mod ++ attrs)

        Ellipse ellipse mod ->
            Ellipse.draw
                ellipse
                (drawOptions mod ++ attrs)

        Line line mod ->
            Line.draw
                line
                (drawOptions mod ++ attrs)

        Group shapes mod ->
            shapes
                |> List.map (draw attrs)
                |> Svg.g (drawOptions mod ++ attrs)


transform : Modifire -> Mat4
transform { translate, rotate, scale } =
    let
        trans =
            mat4
                (Vec4.vec4 1 0 (Vec2.getX translate) 0)
                (Vec4.vec4 0 1 (Vec2.getY translate) 0)
                (Vec4.vec4 0 0 1 0)
                (Vec4.vec4 0 0 0 1)

        scaleing =
            mat4
                (Vec4.vec4 (Vec2.getX scale) 0 0 0)
                (Vec4.vec4 0 (Vec2.getY scale) 0 0)
                (Vec4.vec4 0 0 1 0)
                (Vec4.vec4 0 0 0 1)

        rot =
            mat4
                (Vec4.vec4 (cos rotate) -(sin rotate) 0 0)
                (Vec4.vec4 (sin rotate) (cos rotate) 0 0)
                (Vec4.vec4 0 0 1 0)
                (Vec4.vec4 0 0 0 1)
    in
    scaleing
        |> Mat4.mulAffine rot
        |> Mat4.mulAffine trans


transformToString : Mat4 -> String
transformToString ctm =
    let
        recode =
            Mat4.toRecord ctm

        elements =
            [ recode.m11
            , recode.m21
            , recode.m12
            , recode.m22
            , recode.m13
            , recode.m23
            ]
                |> List.map String.fromFloat
                |> String.join ","
    in
    "matrix("
        ++ elements
        ++ ")"


mat4 : Vec4 -> Vec4 -> Vec4 -> Vec4 -> Mat4
mat4 m1 m2 m3 m4 =
    { m11 = Vec4.getX m1
    , m12 = Vec4.getY m1
    , m13 = Vec4.getZ m1
    , m14 = Vec4.getW m1
    , m21 = Vec4.getX m2
    , m22 = Vec4.getY m2
    , m23 = Vec4.getZ m2
    , m24 = Vec4.getW m2
    , m31 = Vec4.getX m3
    , m32 = Vec4.getY m3
    , m33 = Vec4.getZ m3
    , m34 = Vec4.getW m3
    , m41 = Vec4.getX m4
    , m42 = Vec4.getY m4
    , m43 = Vec4.getZ m4
    , m44 = Vec4.getW m4
    }
        |> Mat4.fromRecord
