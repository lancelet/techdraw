module Main exposing (..)

{-| Draw a rectangle!

All this is to draw a rectangle.

-}

import Browser
import Color exposing (..)
import Html exposing (..)
import Techdraw exposing (Drawing, fill, path, render, stroke, strokeWidth, transform)
import Techdraw.Internal.Util exposing (unsafeForceMaybe)
import Techdraw.Math exposing (Scale(..), affScale)
import Techdraw.PathBuilder as PathBuilder exposing (close, createPath, lineTo, moveTo)
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (height, viewBox, width)
import TypedSvg.Types exposing (Paint(..), px)


type Model
    = Model
        { color : Color
        }


type Msg
    = MouseEnter
    | MouseExit


init : Model
init =
    Model { color = red }


rectangle =
    PathBuilder.empty
        |> moveTo ( 2, 2 )
        |> lineTo ( 2, 8 )
        |> lineTo ( 8, 8 )
        |> lineTo ( 8, 2 )
        |> close
        |> createPath
        |> unsafeForceMaybe "Path should be valid."


drawing : Model -> Drawing msg
drawing (Model model) =
    path rectangle
        |> fill (Paint model.color)
        |> stroke (Paint black)
        |> strokeWidth 2
        |> transform (affScale <| Scale 2 2)
        |> transform (affScale <| Scale 5 5)


view : Model -> Html Msg
view model =
    svg
        [ width (px 100)
        , height (px 100)
        , viewBox 0 0 100 100
        ]
        [ drawing model |> render ]


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        MouseEnter ->
            Model { model | color = red }

        MouseExit ->
            Model { model | color = blue }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
