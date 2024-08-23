module Main exposing (..)

{-| Draw a rectangle!

All this is to draw a rectangle.

-}

import Browser
import Color exposing (..)
import Html exposing (..)
import Techdraw
    exposing
        ( CSysName(..)
        , Drawing
        , fill
        , group
        , onMouseEnter
        , onMouseLeave
        , onMouseMove
        , path
        , render
        , rotateAbout
        , skewX
        , stroke
        , strokeWidth
        , tagCSys
        , transform
        , translate
        )
import Techdraw.Math exposing (P2, Scale(..), affScale, p2x, p2y)
import Techdraw.Shapes.Simple exposing (circle, ellipse, rectRounded)
import TypedSvg.Types exposing (Paint(..))


type Model
    = Model
        { color : Color
        , coords : Maybe P2
        }


type Msg
    = MouseEnter
    | MouseLeave
    | MouseMove P2


init : Model
init =
    Model { color = blue, coords = Nothing }


rectangle =
    let
        r =
            rectRounded
                { x = 2
                , y = 2
                , width = 6
                , height = 6
                , rx = 1
                , ry = 1
                }
    in
    r


drawing : Model -> Drawing Msg
drawing (Model model) =
    group
        [ path
            rectangle
            |> fill (Paint model.color)
            |> stroke (Paint black)
            |> strokeWidth 2
            |> transform (affScale <| Scale 10 10)
            |> tagCSys (CSysName "eventCSys")
            |> translate ( -50, -50 )
            |> skewX 10
            |> translate ( 50, 50 )
            |> rotateAbout 10 ( 50, 50 )
            |> onMouseEnter (\_ -> MouseEnter)
            |> onMouseLeave (\_ -> MouseLeave)
            |> onMouseMove
                (\info ->
                    MouseMove <| info.pointIn (CSysName "eventCSys")
                )
        , path (circle { r = 40, cx = 50, cy = 50 })
            |> fill PaintNone
            |> stroke (Paint green)
            |> strokeWidth 5
        , path (ellipse { rx = 20, ry = 40, cx = 50, cy = 50 })
            |> fill PaintNone
            |> stroke (Paint purple)
            |> strokeWidth 3
        ]


floatToStrRounded : Float -> String
floatToStrRounded value =
    toFloat (round (value * 10)) / 10 |> String.fromFloat


view : Model -> Html Msg
view model =
    let
        (Model m) =
            model

        viewBox =
            { minX = 0
            , minY = 0
            , width = 100
            , height = 100
            }

        coordStr =
            case m.coords of
                Nothing ->
                    ""

                Just pt ->
                    "("
                        ++ floatToStrRounded (p2x pt)
                        ++ ", "
                        ++ floatToStrRounded (p2y pt)
                        ++ ")"
    in
    div []
        [ drawing model |> render viewBox
        , div [] [ p [] [ text coordStr ] ]
        ]


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        MouseEnter ->
            Model { model | color = red }

        MouseLeave ->
            Model
                { model
                    | color = blue
                    , coords = Nothing
                }

        MouseMove pt ->
            Model { model | coords = Just pt }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
