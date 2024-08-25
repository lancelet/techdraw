module Main exposing (..)

{-| Draw an interactive spring!

-}

import Browser
import Color
import Html exposing (Html)
import Techdraw as TD exposing (Drawing)
import Techdraw.Math as Math
import Techdraw.Shapes.Spring exposing (spring)
import Techdraw.Widgets.DragPt as DragPt
import TypedSvg.Types
    exposing
        ( Paint(..)
        , StrokeLinecap(..)
        , StrokeLinejoin(..)
        )


type Model
    = Model
        { pt1 : DragPt.Model
        , pt2 : DragPt.Model
        }


type Msg
    = MsgPt1 DragPt.Msg
    | MsgPt2 DragPt.Msg


init : Model
init =
    Model
        { pt1 = DragPt.init DragPt.defaultStyle (Math.p2 50 300)
        , pt2 = DragPt.init DragPt.defaultStyle (Math.p2 550 300)
        }


drawing : Model -> Drawing Msg
drawing (Model model) =
    TD.group
        [ TD.path
            (spring
                { start = model.pt1.location
                , end = model.pt2.location
                , restLength = 500
                , restCoilHeight = 20
                , restCoilWidth = 300
                , coilCount = 10
                , minEndLength = 10
                , minCoilAngleDeg = 10
                , maxCoilAngleDeg = 88
                }
            )
            |> TD.fill PaintNone
            |> TD.stroke (Paint Color.black)
            |> TD.strokeWidth 2
            |> TD.strokeLinecap StrokeLinecapButt
            |> TD.strokeLinejoin StrokeLinejoinRound
        , DragPt.view model.pt1 |> TD.map MsgPt1
        , DragPt.view model.pt2 |> TD.map MsgPt2
        ]


view : Model -> Html Msg
view model =
    let
        viewBox =
            { minX = 0
            , minY = 0
            , width = 600
            , height = 600
            }
    in
    Html.div [] [ drawing model |> TD.render viewBox ]


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        MsgPt1 p1msg ->
            Model { model | pt1 = DragPt.updateModel p1msg model.pt1 }

        MsgPt2 p2msg ->
            Model { model | pt2 = DragPt.updateModel p2msg model.pt2 }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
