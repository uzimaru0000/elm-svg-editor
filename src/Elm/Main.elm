module Main exposing (main)

import Browser exposing (..)
import Browser.Dom exposing (..)
import Browser.Events as Browser exposing (onResize)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Json.Decode as JD
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Shape exposing (..)
import Shape.Rect as Rect
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Svg.Events as Svg
import Task


type alias Model =
    { shapes : List Shape
    , canvasPos : Vec2
    , screenSize : Vec2
    , currentShape : Maybe Shape
    , startPos : Maybe Vec2
    , endPos : Maybe Vec2
    }


type Msg
    = NoOp
    | GetScreenSize Int Int
    | GetCanvasPos (Result Error ( Float, Float ))
    | Select Shape
    | MouseDown Vec2
    | MouseMove Vec2
    | MouseUp Vec2


main : Program () Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        conv v =
            GetScreenSize (v.viewport.width |> round) (v.viewport.height |> round)
    in
    ( { shapes = []
      , screenSize = vec2 0 0
      , canvasPos = vec2 0 0
      , startPos = Nothing
      , endPos = Nothing
      , currentShape = Nothing
      }
    , getViewport
        |> Task.perform conv
    )


view : Model -> Document Msg
view model =
    { title = "SvgEditor"
    , body =
        let
            width =
                Vec2.getX model.screenSize - Vec2.getX model.canvasPos

            height =
                Vec2.getY model.screenSize - Vec2.getY model.canvasPos
        in
        [ Html.div
            [ Html.style "background" "red"
            , Html.style "width" "100%"
            , Html.style "height" "32px"
            ]
            [ Html.text "Header" ]
        , Svg.svg
            [ width
                |> String.fromFloat
                |> Svg.width
            , height
                |> String.fromFloat
                |> Svg.height
            , [ 0, 0, width, height ]
                |> List.map String.fromFloat
                |> String.join " "
                |> Svg.viewBox
            , onMouseDownWithPosition MouseDown
            , onMouseMoveWithPosition MouseMove
            , onMouseUpWithPosition MouseUp
            , Html.id "display"
            ]
            ([ model.shapes
                |> List.map (\s -> Shape.draw [] s)
             , model.currentShape
                |> Maybe.map (Shape.draw [])
                |> Maybe.withDefault (Svg.text "")
                |> List.singleton
             ]
                |> List.concat
            )
        ]
    }


vec2Decoder : JD.Decoder Vec2
vec2Decoder =
    JD.map2 vec2
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


onMouseDownWithPosition : (Vec2 -> Msg) -> Svg.Attribute Msg
onMouseDownWithPosition msg =
    Svg.on "mousedown" <|
        JD.map msg vec2Decoder


onMouseMoveWithPosition : (Vec2 -> Msg) -> Svg.Attribute Msg
onMouseMoveWithPosition msg =
    Svg.on "mousemove" <|
        JD.map msg vec2Decoder


onMouseUpWithPosition : (Vec2 -> Msg) -> Svg.Attribute Msg
onMouseUpWithPosition msg =
    Svg.on "mouseup" <|
        JD.map msg vec2Decoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        relPos pos =
            Vec2.sub pos model.canvasPos
    in
    case msg of
        GetScreenSize width height ->
            ( { model | screenSize = vec2 (toFloat width) (toFloat height) }
            , getElement "display"
                |> Task.map .element
                |> Task.map (\v -> ( v.x, v.y ))
                |> Task.attempt GetCanvasPos
            )

        GetCanvasPos (Ok ( x, y )) ->
            ( { model | canvasPos = vec2 x y }
            , Cmd.none
            )

        MouseDown pos ->
            ( { model
                | startPos = Just (relPos pos)
                , endPos = Just (relPos pos)
                , currentShape =
                    { modifire | translate = relPos pos }
                        |> Rect (Rect.create (relPos pos) (relPos pos))
                        |> Just
              }
            , Cmd.none
            )

        MouseMove pos ->
            ( { model
                | endPos = Just (relPos pos)
                , currentShape =
                    Maybe.map3 shapeUpdate model.startPos (Just <| relPos pos) model.currentShape
              }
            , Cmd.none
            )

        MouseUp pos ->
            ( { model
                | endPos = Nothing
                , currentShape = Nothing
                , shapes =
                    Maybe.map3 shapeUpdate model.startPos (Just <| relPos pos) model.currentShape
                        |> Maybe.map List.singleton
                        |> Maybe.map ((++) model.shapes)
                        |> Maybe.withDefault []
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


shapeUpdate : Vec2 -> Vec2 -> Shape -> Shape
shapeUpdate start end shape =
    case shape of
        Rect rect modifire ->
            { modifire
                | translate =
                    Vec2.sub end start
                        |> Vec2.scale 0.5
                        |> Vec2.add start
                , fill = Just "#199861"
            }
                |> Rect (Rect.create start end)

        _ ->
            shape


focusShape : List Shape -> Shape -> List Shape
focusShape shapeList target =
    shapeList
        |> List.filter ((/=) target)


subscriptions : Model -> Sub Msg
subscriptions model =
    [ onResize GetScreenSize
    ]
        |> Sub.batch
