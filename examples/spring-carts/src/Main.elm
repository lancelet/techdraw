module Main exposing (..)

{-| Draw a spring-cart system.

-}

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color exposing (Color)
import Html exposing (Html)
import Techdraw as TD exposing (Drawing)
import Techdraw.Math as M exposing (P2)
import Techdraw.PathBuilder as PB
import Techdraw.Shapes.Simple exposing (circle, rect)
import Techdraw.Shapes.Spring exposing (spring)
import TypedSvg.Types exposing (Paint(..), StrokeLinejoin(..))


type Model
    = Model
        { dragging : Maybe ( Int, P2 )
        , cartOver : Maybe Int
        , cartState : CartState
        }


type Msg
    = MsgCartEnter Int
    | MsgCartLeave
    | MsgStartDrag Int P2
    | MsgMidDrag P2
    | MsgEndDrag
    | MsgAnimFrame Float


init : Model
init =
    Model
        { dragging = Nothing
        , cartOver = Nothing
        , cartState =
            initCartState
                { cart1X = 0
                , cart2X = 0
                , cart3X = 0
                , cart1V = 0
                , cart2V = 0
                , cart3V = 0
                }
        }


cartColor : Int -> Model -> Color
cartColor i (Model model) =
    let
        nonDraggingColor : () -> Color
        nonDraggingColor _ =
            case model.cartOver of
                Just j ->
                    if i == j then
                        Color.lightGreen

                    else
                        Color.lightGray

                Nothing ->
                    Color.lightGray
    in
    case model.dragging of
        Just ( j, _ ) ->
            if i == j then
                Color.lightOrange

            else
                Color.lightGray

        Nothing ->
            nonDraggingColor ()


cart1Start =
    200


cart2Start =
    400


cart3Start =
    600


drawing : Model -> Drawing Msg
drawing (Model model) =
    let
        groundY =
            60

        cart1Fill =
            cartColor 1 (Model model)

        cart2Fill =
            cartColor 2 (Model model)

        cart3Fill =
            cartColor 3 (Model model)

        cart1x =
            csGetCart1X model.cartState + cart1Start

        cart2x =
            csGetCart2X model.cartState + cart2Start

        cart3x =
            csGetCart3X model.cartState + cart3Start
    in
    TD.group
        [ TD.tagCSys (TD.CSysName "drawing")

        -- Ground plane
        , TD.path (rect { x = 50, y = groundY - 15, width = 720, height = 15 })
            |> TD.fill (Paint Color.gray)
        , TD.path
            (PB.empty
                |> PB.moveTo ( 50, groundY )
                |> PB.lineTo ( 770, groundY )
                |> PB.createPath
            )
            |> TD.stroke (Paint Color.black)
            |> TD.strokeWidth 3

        -- Left wall
        , TD.path (rect { x = 80 - 15, y = 180, width = 15, height = 270 - 180 })
            |> TD.fill (Paint Color.gray)
        , TD.path
            (PB.empty
                |> PB.moveTo ( 80, 180 )
                |> PB.lineTo ( 80, 270 )
                |> PB.createPath
            )
            |> TD.stroke (Paint Color.black)
            |> TD.strokeWidth 3
        , TD.dropAnchor "wall.right" ( 80, (270 + 180) / 2 )

        -- Carts
        , cart { height = 250, fillColor = cart1Fill }
            |> TD.translate ( cart1x, groundY )
            |> TD.prependAnchorNamespace "cart1"
            |> TD.onMouseEnter (\_ -> MsgCartEnter 1)
            |> TD.onMouseLeave (\_ -> MsgCartLeave)
            |> TD.onMouseDown (\minfo -> MsgStartDrag 1 (minfo.pointIn <| TD.CSysName "drawing"))
        , cart { height = 150, fillColor = cart2Fill }
            |> TD.translate ( cart2x, groundY )
            |> TD.prependAnchorNamespace "cart2"
            |> TD.onMouseEnter (\_ -> MsgCartEnter 2)
            |> TD.onMouseLeave (\_ -> MsgCartLeave)
            |> TD.onMouseDown (\minfo -> MsgStartDrag 2 (minfo.pointIn <| TD.CSysName "drawing"))
        , cart { height = 250, fillColor = cart3Fill }
            |> TD.translate ( cart3x, groundY )
            |> TD.prependAnchorNamespace "cart3"
            |> TD.onMouseEnter (\_ -> MsgCartEnter 3)
            |> TD.onMouseLeave (\_ -> MsgCartLeave)
            |> TD.onMouseDown (\minfo -> MsgStartDrag 3 (minfo.pointIn <| TD.CSysName "drawing"))

        -- Springs
        , TD.weighAnchors
            (\getAnchor ->
                let
                    ( cart1LeftX, _ ) =
                        getAnchor "cart1.left"

                    ( cart1RightX, _ ) =
                        getAnchor "cart1.right"

                    ( cart2LeftX, _ ) =
                        getAnchor "cart2.left"

                    ( cart2RightX, _ ) =
                        getAnchor "cart2.right"

                    ( cart3LeftX, _ ) =
                        getAnchor "cart3.left"

                    ( wallRightX, wallRightY ) =
                        getAnchor "wall.right"
                in
                TD.group
                    [ TD.path
                        (spring
                            { start = M.p2 cart1RightX 290
                            , end = M.p2 cart3LeftX 290
                            , restLength = 400
                            , restCoilHeight = 15
                            , restCoilWidth = 200
                            , coilCount = 8
                            , minEndLength = 5
                            , minCoilAngleDeg = 5
                            , maxCoilAngleDeg = 88
                            }
                        )
                        |> TD.fill PaintNone
                        |> TD.stroke (Paint Color.black)
                        |> TD.strokeWidth 2
                        |> TD.strokeLinejoin StrokeLinejoinRound
                    , TD.path
                        (spring
                            { start = M.p2 cart1RightX 160
                            , end = M.p2 cart2LeftX 160
                            , restLength = 200
                            , restCoilHeight = 15
                            , restCoilWidth = 100
                            , coilCount = 4
                            , minEndLength = 5
                            , minCoilAngleDeg = 5
                            , maxCoilAngleDeg = 88
                            }
                        )
                        |> TD.fill PaintNone
                        |> TD.stroke (Paint Color.black)
                        |> TD.strokeWidth 2
                        |> TD.strokeLinejoin StrokeLinejoinRound
                    , TD.path
                        (spring
                            { start = M.p2 cart2RightX 160
                            , end = M.p2 cart3LeftX 160
                            , restLength = 200
                            , restCoilHeight = 15
                            , restCoilWidth = 100
                            , coilCount = 4
                            , minEndLength = 5
                            , minCoilAngleDeg = 5
                            , maxCoilAngleDeg = 88
                            }
                        )
                        |> TD.fill PaintNone
                        |> TD.stroke (Paint Color.black)
                        |> TD.strokeWidth 2
                        |> TD.strokeLinejoin StrokeLinejoinRound
                    , TD.path
                        (spring
                            { start = M.p2 wallRightX wallRightY
                            , end = M.p2 cart1LeftX wallRightY
                            , restLength = 150
                            , restCoilHeight = 15
                            , restCoilWidth = 100
                            , coilCount = 3
                            , minEndLength = 5
                            , minCoilAngleDeg = 5
                            , maxCoilAngleDeg = 88
                            }
                        )
                        |> TD.fill PaintNone
                        |> TD.stroke (Paint Color.black)
                        |> TD.strokeWidth 2
                        |> TD.strokeLinejoin StrokeLinejoinRound
                    ]
            )
        ]
        |> (\dwg ->
                case model.dragging of
                    Nothing ->
                        dwg

                    Just _ ->
                        dwg
                            |> TD.onHostMouseUp (\_ -> MsgEndDrag)
                            |> TD.onHostMouseLeave (\_ -> MsgEndDrag)
                            |> TD.onHostMouseMove
                                (\minfo ->
                                    if minfo.buttons.button1 == TD.MouseButtonPressed then
                                        MsgMidDrag <| minfo.pointIn <| TD.CSysName "drawing"

                                    else
                                        MsgEndDrag
                                )
           )


view : Model -> Html Msg
view model =
    let
        (Model m) =
            model

        viewBox =
            { minX = 0
            , minY = 0
            , width = 800
            , height = 400
            }
    in
    Html.div []
        [ drawing model |> TD.render viewBox ]


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        MsgCartEnter i ->
            Model { model | cartOver = Just i }

        MsgCartLeave ->
            Model { model | cartOver = Nothing }

        MsgStartDrag i p ->
            Model
                { model
                    | dragging = Just ( i, p )
                }

        MsgMidDrag p ->
            let
                ( i, _ ) =
                    Maybe.withDefault ( 1, M.p2 0 0 ) model.dragging
            in
            Model
                { model
                    | dragging = Just ( i, p )
                }

        MsgEndDrag ->
            Model
                { model
                    | dragging = Nothing
                }

        MsgAnimFrame dt ->
            let
                -- Stop large jumps when the window loses focus.
                clampedDt =
                    M.clamp ( 0, 100 ) dt
            in
            Model
                { model
                    | cartState =
                        updateCartState
                            model.dragging
                            clampedDt
                            model.cartState
                }


main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta MsgAnimFrame
                    ]
        }



---- Drawing a Cart -----------------------------------------------------------


cart :
    { height : Float
    , fillColor : Color
    }
    -> Drawing msg
cart r =
    let
        wheelRadius =
            8

        baseWidth =
            50

        memberWidth =
            15

        wheel =
            TD.path
                (circle
                    { cx = 0
                    , cy = 0
                    , r = wheelRadius
                    }
                )
                |> TD.fill PaintNone
                |> TD.stroke (Paint Color.black)
                |> TD.strokeWidth 2
    in
    TD.group
        [ TD.group
            [ wheel |> TD.translate ( -baseWidth / 2 + wheelRadius, 0 )
            , wheel |> TD.translate ( baseWidth / 2 - wheelRadius, 0 )
            ]
            |> TD.translate ( 0, wheelRadius )
        , TD.path
            (PB.empty
                |> PB.moveTo ( -baseWidth / 2, 0 )
                |> PB.lineTo ( baseWidth / 2, 0 )
                |> PB.lineTo ( baseWidth / 2, memberWidth )
                |> PB.lineTo ( memberWidth / 2, memberWidth )
                |> PB.lineTo ( memberWidth / 2, r.height )
                |> PB.lineTo ( -memberWidth / 2, r.height )
                |> PB.lineTo ( -memberWidth / 2, memberWidth )
                |> PB.lineTo ( -baseWidth / 2, memberWidth )
                |> PB.close
                |> PB.createPath
            )
            |> TD.fill (Paint r.fillColor)
            |> TD.stroke (Paint Color.black)
            |> TD.strokeWidth 2
            |> TD.translate ( 0, 2 * wheelRadius )
        , TD.dropAnchor "left" ( -memberWidth / 2, 0 )
        , TD.dropAnchor "right" ( memberWidth / 2, 0 )
        ]



---- CartState ----------------------------------------------------------------


type CartState
    = CartState (Array Float)


initCartState :
    { cart1X : Float
    , cart2X : Float
    , cart3X : Float
    , cart1V : Float
    , cart2V : Float
    , cart3V : Float
    }
    -> CartState
initCartState r =
    CartState <|
        Array.fromList
            [ r.cart1X
            , r.cart2X
            , r.cart3X
            , r.cart1V
            , r.cart2V
            , r.cart3V
            ]


csGetCart1X : CartState -> Float
csGetCart1X cs =
    csGet 0 cs


csGetCart2X : CartState -> Float
csGetCart2X cs =
    csGet 1 cs


csGetCart3X : CartState -> Float
csGetCart3X cs =
    csGet 2 cs


csGetCart1V : CartState -> Float
csGetCart1V cs =
    csGet 3 cs


csGetCart2V : CartState -> Float
csGetCart2V cs =
    csGet 4 cs


csGetCart3V : CartState -> Float
csGetCart3V cs =
    csGet 5 cs


csGet : Int -> CartState -> Float
csGet i (CartState s) =
    Array.get i s |> Maybe.withDefault 0


csSet : Int -> Float -> CartState -> CartState
csSet i x (CartState s) =
    CartState <| Array.set i x s


updateCartState : Maybe ( Int, P2 ) -> Float -> CartState -> CartState
updateCartState maybeDragInfo dt cs =
    let
        k =
            2.0e-5

        c =
            4.5e-4

        k1 =
            k * 3

        k2 =
            k

        k3 =
            k * 0.3

        k4 =
            k * 0.3

        c1 =
            c * 2

        c2 =
            c

        c3 =
            c

        c4 =
            c * 0.6

        m =
            0.2

        ( u1, u2, u3 ) =
            ( csGetCart1X cs, csGetCart2X cs, csGetCart3X cs )

        ( v1, v2, v3 ) =
            ( csGetCart1V cs, csGetCart2V cs, csGetCart3V cs )

        f1Internal =
            (-k1 - k2 - k3) * u1 + k3 * u2 + k2 * u3 + (-c1 - c2 - c3) * v1 + c3 * v2 + c2 * v3

        f2Internal =
            k3 * u1 + (-k3 - k4) * u2 + k4 * u3 + c3 * v1 + (-c3 - c4) * v2 + c4 * v3

        f3Internal =
            k2 * u1 + k4 * u2 + (-k2 - k4) * u3 + c2 * v1 + c4 * v2 + (-c2 - c4) * v3

        ( f1, f2, f3 ) =
            case maybeDragInfo of
                Nothing ->
                    ( 0, 0, 0 )

                Just ( i, p ) ->
                    let
                        cartX =
                            csGet (i - 1) cs
                                + (case i of
                                    1 ->
                                        cart1Start

                                    2 ->
                                        cart2Start

                                    3 ->
                                        cart3Start

                                    _ ->
                                        0
                                  )

                        dx =
                            M.p2x p - cartX

                        kDrag =
                            0.000015

                        f =
                            kDrag * dx
                    in
                    case i of
                        1 ->
                            ( f, 0, 0 )

                        2 ->
                            ( 0, f, 0 )

                        3 ->
                            ( 0, 0, f )

                        _ ->
                            ( 0, 0, 0 )

        accel =
            initCartState
                { cart1X = csGetCart1V cs
                , cart2X = csGetCart2V cs
                , cart3X = csGetCart3V cs
                , cart1V = (f1 + f1Internal) / m
                , cart2V = (f2 + f2Internal) / m
                , cart3V = (f3 + f3Internal) / m
                }
    in
    step cartStateNIType dt accel cs


cartStateNIType : NIType CartState
cartStateNIType =
    NIType
        { scale =
            \s (CartState cs) ->
                cs
                    |> Array.toList
                    |> List.map (\x -> s * x)
                    |> Array.fromList
                    |> CartState
        , add =
            \(CartState l) (CartState r) ->
                List.map2
                    (\lx rx -> lx + rx)
                    (Array.toList l)
                    (Array.toList r)
                    |> Array.fromList
                    |> CartState
        }



---- Numerical Integration ----------------------------------------------------


type NIType a
    = NIType
        { scale : Float -> a -> a
        , add : a -> a -> a
        }


step : NIType a -> Float -> a -> a -> a
step (NIType ops) dt accel x =
    ops.add x (ops.scale dt accel)
