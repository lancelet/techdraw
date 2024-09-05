module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Techdraw as TD exposing (Drawing)
import Techdraw.Math as Math exposing (p2)
import Techdraw.Shapes.Simple exposing (circle, rectRounded)
import Techdraw.Style as Style exposing (LinearGradient, Stop(..))
import Techdraw.Types as TT
import TypedSvg.Attributes exposing (rotate)


type Model
    = Model
        { translateX : Float
        , translateY : Float
        , rotate : Float
        , scaleX : Float
        , scaleY : Float
        , skewX : Float
        , mouseOverInnerCircle : Bool
        }


type Msg
    = MsgReset
    | MsgTranslateXChanged Float
    | MsgTranslateYChanged Float
    | MsgRotateChanged Float
    | MsgScaleXChanged Float
    | MsgScaleYChanged Float
    | MsgSkewXChanged Float
    | MsgMouseEnter
    | MsgMouseLeave


init : Model
init =
    Model
        { translateX = 0
        , translateY = 0
        , rotate = 0
        , scaleX = 1
        , scaleY = 1
        , skewX = 0
        , mouseOverInnerCircle = False
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


lg : LinearGradient
lg =
    Style.linearGradient
        { start = p2 (halfWidth - circleRadius) (halfHeight - circleRadius)
        , end = p2 (halfWidth + circleRadius) (halfHeight + circleRadius)
        , transform = Math.affIdentity
        , gradient =
            Style.gradient
                [ Stop 0 Color.green
                , Stop 1 Color.yellow
                ]
        }


lg2 : LinearGradient
lg2 =
    Style.linearGradient
        { start = p2 (halfWidth - circleRadius) (halfHeight + circleRadius)
        , end = p2 (halfWidth + circleRadius) (halfHeight - circleRadius)
        , transform = Math.affIdentity
        , gradient =
            Style.gradient
                [ Stop 0 Color.purple
                , Stop 1 Color.green
                ]
        }


drawing : Model -> Drawing Msg
drawing (Model model) =
    TD.atop
        (TD.below
            (TD.path
                (circle
                    { r = circleRadius
                    , cx = drawingWidth / 2
                    , cy = drawingHeight / 2
                    }
                )
            )
            (TD.path
                (circle
                    { r = circleRadius / 3
                    , cx = drawingWidth / 2
                    , cy = drawingHeight / 2
                    }
                )
                |> TD.fill
                    (if model.mouseOverInnerCircle then
                        Style.Paint Color.darkOrange

                     else
                        Style.Paint Color.darkRed
                    )
                |> TD.onMouseEnter (\_ -> MsgMouseEnter)
                |> TD.onMouseLeave (\_ -> MsgMouseLeave)
            )
        )
        (TD.path
            (rectRounded
                { x = halfWidth - circleRadius
                , y = halfHeight - circleRadius
                , width = 2 * circleRadius
                , height = 2 * circleRadius
                , rx = 0.2 * circleRadius
                , ry = 0.2 * circleRadius
                }
            )
            |> TD.fill (Style.PaintLinearGradient lg2)
        )
        |> TD.fill (Style.PaintLinearGradient lg)
        |> TD.stroke (Style.Paint Color.black)
        |> TD.strokeWidth 0.2
        |> (-- Scale and Skew about the middle of the drawing
            TD.translate (Math.v2 -halfWidth -halfHeight)
                >> TD.scale model.scaleX model.scaleY
                >> TD.skewXDegrees model.skewX
                >> TD.translate (Math.v2 halfWidth halfHeight)
           )
        |> TD.translate (Math.v2 model.translateX model.translateY)
        |> TD.rotateDegreesAbout model.rotate (Math.p2 halfWidth halfHeight)


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

        MsgMouseEnter ->
            Model { model | mouseOverInnerCircle = True }

        MsgMouseLeave ->
            Model { model | mouseOverInnerCircle = False }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
