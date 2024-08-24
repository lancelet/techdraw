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
        , dropAnchor
        , fill
        , group
        , onMouseEnter
        , onMouseLeave
        , path
        , prependAnchorNamespace
        , render
        , scale
        , stroke
        , strokeWidth
        , tagCSys
        , weighAnchors
        )
import Techdraw.Math exposing (P2, Scale(..), p2x, p2y)
import Techdraw.PathBuilder as PB
import Techdraw.Shapes.Simple exposing (rect, rectRounded)
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


drawing : Model -> Drawing Msg
drawing (Model model) =
    group
        [ group
            [ tagCSys (CSysName "eventCSys")
            , path
                (rect { x = 0, y = 0, width = 60, height = 60 })
                |> fill (Paint Color.lightGreen)
            , group
                [ path
                    (rectRounded
                        { x = 5
                        , y = 20
                        , width = 20
                        , height = 20
                        , rx = 5
                        , ry = 5
                        }
                    )
                    |> fill (Paint model.color)
                    |> onMouseEnter (\_ -> MouseEnter)
                    |> onMouseLeave (\_ -> MouseLeave)
                , dropAnchor "right" ( 25, 30 )
                ]
                |> prependAnchorNamespace "leftBox"
            , group
                [ path
                    (rectRounded
                        { x = 60 - 5 - 20
                        , y = 20
                        , width = 20
                        , height = 20
                        , rx = 5
                        , ry = 5
                        }
                    )
                    |> fill (Paint Color.orange)
                , dropAnchor "left" ( 60 - 5 - 20, 30 )
                ]
                |> prependAnchorNamespace "rightBox"
            ]
            |> scale ( 10, 10 )
        , group
            [ weighAnchors <|
                \getAnchor ->
                    path
                        (PB.empty
                            |> PB.moveTo (getAnchor "leftBox.right")
                            |> PB.lineTo (getAnchor "rightBox.left")
                            |> PB.createPath
                        )
                        |> stroke (Paint Color.purple)
                        |> strokeWidth 20
            ]
            |> scale ( 100, 100 )
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
            , width = 600
            , height = 600
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
