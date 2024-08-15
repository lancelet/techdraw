module Main exposing (..)

{-| Draw a rectangle!

All this is to draw a rectangle.

-}

import Browser
import Color exposing (..)
import Html exposing (..)
import Techdraw exposing (Drawing, path, render, withFill, transform)
import Techdraw.Math exposing (affScale, Scale(..))
import Techdraw.Internal.Util exposing (unsafeForceMaybe)
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
    Model { color = black }


rectangle =
    PathBuilder.empty
        |> moveTo ( 2, 2 )
        |> lineTo ( 2, 8 )
        |> lineTo ( 8, 8 )
        |> lineTo ( 8, 2 )
        |> close
        |> createPath
        |> unsafeForceMaybe "Path should be valid."


drawing : Drawing msg
drawing =
    path rectangle
        |> withFill (Paint red)
        |> transform (affScale <| Scale 2 2)
        |> transform (affScale <| Scale 5 5)


view : Model -> Html Msg
view (Model model) =
    svg
        [ width (px 100)
        , height (px 100)
        , viewBox 0 0 100 100
        ]
        [ render drawing ]


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        MouseEnter ->
            Model { model | color = red }

        MouseExit ->
            Model { model | color = black }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
