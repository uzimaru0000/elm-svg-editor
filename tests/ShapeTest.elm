module ShapeTest exposing (matrixTest)

import Expect
import Fuzz exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (vec2)
import Math.Vector4 exposing (Vec4, vec4)
import Shape exposing (..)
import Test exposing (..)


noTransform : String
noTransform =
    "matrix(1,0,0,1,0,0)"


matrixTest : Test
matrixTest =
    describe "matrix"
        [ test "make mat4" <|
            \_ ->
                Shape.mat4
                    (vec4 1 0 0 0)
                    (vec4 0 1 0 0)
                    (vec4 0 0 1 0)
                    (vec4 0 0 0 1)
                    |> Expect.equal Mat4.identity
        , test "matrix to string" <|
            \_ ->
                Shape.mat4
                    (vec4 1 0 0 0)
                    (vec4 0 1 0 0)
                    (vec4 0 0 1 0)
                    (vec4 0 0 0 1)
                    |> Shape.transformToString
                    |> Expect.equal "matrix(1,0,0,1,0,0)"
        , test "no move" <|
            \_ ->
                Shape.transform (vec2 0 0) 0 (vec2 1 1)
                    |> Shape.transformToString
                    |> Expect.equal noTransform
        , test "move (10, 10)" <|
            \_ ->
                Shape.transform (vec2 10 0) 0 (vec2 1 1)
                    |> Shape.transformToString
                    |> Expect.equal "matrix(1,0,0,1,10,0)"
        , test "rotate PI" <|
            \_ ->
                Shape.transform (vec2 0 0) pi (vec2 1 1)
                    |> Shape.transformToString
                    |> Expect.equal
                        ("matrix("
                            ++ (List.map String.fromFloat >> String.join ",") [ cos pi, sin pi, -1 * sin pi, cos pi, 0, 0 ]
                            ++ ")"
                        )
        , test "scale (2, 2)" <|
            \_ ->
                Shape.transform (vec2 0 0) 0 (vec2 2 2)
                    |> Shape.transformToString
                    |> Expect.equal "matrix(2,0,0,2,0,0)"
        ]
