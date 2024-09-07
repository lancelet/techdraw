module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Techdraw as TD exposing (Drawing)
import Techdraw.Event exposing (MouseInfo(..))
import Techdraw.Math as Math exposing (P2, p2)
import Techdraw.Shapes.Simple exposing (circle, rectRounded)
import Techdraw.Style as Style exposing (LinearGradient, Stop(..))
import Techdraw.Types as TT


type Model
    = Model
        { translateX : Float
        , translateY : Float
        , rotate : Float
        , scaleX : Float
        , scaleY : Float
        , skewX : Float
        , coords : Maybe P2
        }


type Msg
    = MsgReset
    | MsgTranslateXChanged Float
    | MsgTranslateYChanged Float
    | MsgRotateChanged Float
    | MsgScaleXChanged Float
    | MsgScaleYChanged Float
    | MsgSkewXChanged Float
    | MsgMouseMove P2


init : Model
init =
    Model
        { translateX = 0
        , translateY = 0
        , rotate = 0
        , scaleX = 1
        , scaleY = 1
        , skewX = 0
        , coords = Nothing
        }


drawingWidth =
    600


drawingHeight =
    400


halfWidth =
    drawingWidth / 2


halfHeight =
    drawingHeight / 2


circleRadius =
    0.2 * min drawingWidth drawingHeight



-- Bottom-left: Red
-- Top-right: Green


lg : LinearGradient
lg =
    Style.linearGradient
        { start = p2 (halfWidth - circleRadius) (halfHeight - circleRadius)
        , end = p2 (halfWidth + circleRadius) (halfHeight + circleRadius)
        , transform = Math.affIdentity
        , gradient =
            Style.gradient
                [ Stop 0 Color.red
                , Stop 1 Color.green
                ]
        }


rectStyle : Drawing msg -> Drawing msg
rectStyle =
    TD.fill (Style.PaintLinearGradient lg)
        >> TD.stroke (Style.Paint Color.black)
        >> TD.strokeWidth 8


rectTransform : Model -> Drawing msg -> Drawing msg
rectTransform (Model model) =
    (TD.translate (Math.v2 -halfWidth -halfHeight)
        >> TD.scale model.scaleX model.scaleY
        >> TD.skewXDegrees model.skewX
        >> TD.translate (Math.v2 halfWidth halfHeight)
    )
        >> TD.translate (Math.v2 model.translateX model.translateY)
        >> TD.rotateDegreesAbout model.rotate (Math.p2 halfWidth halfHeight)


drawing : Model -> Drawing Msg
drawing (Model model) =
    TD.stack TT.BottomToTop
        [ TD.path
            (rectRounded
                { x = halfWidth - circleRadius
                , y = halfHeight - circleRadius
                , width = 2 * circleRadius
                , height = 2 * circleRadius
                , rx = 0.2 * circleRadius
                , ry = 0.2 * circleRadius
                }
            )
            |> rectStyle
            |> TD.freeze TT.Hidden (Just (TT.FrozenName "frozen_rect"))
        , TD.path
            (rectRounded
                { x = halfWidth - (1 - 0.2) * circleRadius
                , y = halfHeight - (1 - 0.2) * circleRadius
                , width = (2 * (1 - 0.2)) * circleRadius
                , height = (2 * (1 - 0.2)) * circleRadius
                , rx = 0.2 * circleRadius
                , ry = 0.2 * circleRadius
                }
            )
            |> rectStyle
        , TD.stack
            TT.TopToBottom
            [ TD.use (TT.FrozenName "frozen_rect")
                |> TD.translate (Math.v2 (2 * circleRadius + 10) 0)
            , TD.use (TT.FrozenName "frozen_rect")
                |> TD.translate (Math.v2 -(2 * circleRadius + 10) 0)
            ]
            |> TD.freeze TT.Visible (Just (TT.FrozenName "rect_pair"))
        , TD.stack TT.TopToBottom
            [ TD.use (TT.FrozenName "rect_pair")
            , TD.use (TT.FrozenName "frozen_rect")
            ]
            |> TD.freeze TT.Visible (Just (TT.FrozenName "triple"))
            |> TD.translate (Math.v2 0 (2 * circleRadius + 10))
        , TD.use (TT.FrozenName "triple")
            |> TD.translate (Math.v2 0 -(2 * circleRadius + 10))
        , case model.coords of
            Nothing ->
                TD.empty

            Just p ->
                TD.path
                    (circle
                        { cx = Math.p2x p
                        , cy = Math.p2y p
                        , r = 0.1 * circleRadius
                        }
                    )
                    |> TD.fill (Style.Paint Color.red)
        ]
        |> TD.tagCSys (TT.CSysName "original")
        |> rectTransform (Model model)
        |> TD.freeze TT.Visible Nothing
        |> TD.onHostMouseMove
            (\(MouseInfo mouseInfo) ->
                mouseInfo.pointIn (TT.CSysName "original") |> MsgMouseMove
            )


view : Model -> Html Msg
view (Model model) =
    Html.div []
        [ Html.div []
            [ drawing (Model model)
                |> TD.toSvg (TT.sizingWH drawingWidth drawingHeight)
            ]
        , drawSlider "TranslateX"
            model.translateX
            ( -100, 100 )
            MsgTranslateXChanged
        , drawSlider "TranslateY"
            model.translateY
            ( -100, 100 )
            MsgTranslateYChanged
        , drawSlider "Rotation"
            model.rotate
            ( -360, 360 )
            MsgRotateChanged
        , drawSlider "ScaleX"
            model.scaleX
            ( 0.1, 2 )
            MsgScaleXChanged
        , drawSlider "ScaleY"
            model.scaleY
            ( 0.1, 2 )
            MsgScaleYChanged
        , drawSlider "SkewX"
            model.skewX
            ( -80, 80 )
            MsgSkewXChanged
        , Html.div
            [ HA.style "display" "inline-block"
            , HA.style "width" "8em"
            , HA.style "text-align" "right"
            , HA.style "margin-top" "10px"
            ]
            [ Html.button
                [ HE.onClick MsgReset
                ]
                [ Html.text "Reset"
                ]
            ]
        ]


drawSlider : String -> Float -> ( Float, Float ) -> (Float -> Msg) -> Html Msg
drawSlider title value ( minValue, maxValue ) createMessage =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "align-items" "center"
        ]
        [ Html.p
            [ HA.style "display" "inline-block"
            , HA.style "min-width" "8em"
            , HA.style "max-width" "8em"
            , HA.style "width" "8em"
            , HA.style "text-align" "right"
            , HA.style "padding-right" "10px"
            , HA.style "font-family" "sans-serif"
            , HA.style "margin" "0.3ex"
            ]
            [ Html.text title
            ]
        , Html.input
            [ HA.type_ "range"
            , HA.min (String.fromFloat minValue)
            , HA.max (String.fromFloat maxValue)
            , HA.step (String.fromFloat (0.001 * (maxValue - minValue)))
            , HA.style "display" "inline-block"
            , HA.style "min-width" "400px"
            , HA.style "max-width" "400px"
            , HA.style "width" "400px"
            , HA.value (String.fromFloat value)
            , HE.onInput
                (String.toFloat
                    >> Maybe.withDefault 0
                    >> createMessage
                )
            ]
            []
        ]


update : Msg -> Model -> Model
update message (Model model) =
    case message of
        MsgReset ->
            init

        MsgTranslateXChanged tx ->
            Model { model | translateX = tx }

        MsgTranslateYChanged ty ->
            Model { model | translateY = ty }

        MsgRotateChanged angleDeg ->
            Model { model | rotate = angleDeg }

        MsgScaleXChanged scaleX ->
            Model { model | scaleX = scaleX }

        MsgScaleYChanged scaleY ->
            Model { model | scaleY = scaleY }

        MsgSkewXChanged angleDeg ->
            Model { model | skewX = angleDeg }

        MsgMouseMove p ->
            Model { model | coords = Just p }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
