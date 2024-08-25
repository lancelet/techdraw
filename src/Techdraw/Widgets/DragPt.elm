module Techdraw.Widgets.DragPt exposing
    ( Model
    , Msg(..)
    , Style(..)
    , init, defaultStyle
    , update, updateModel
    , view
    )

{-| A draggable-point widget.

@docs Model
@docs Msg
@docs Style
@docs init, defaultStyle
@docs update, updateModel
@docs view

-}

import Color exposing (Color)
import Techdraw as T exposing (Drawing)
import Techdraw.Math as Math exposing (P2)
import Techdraw.Shapes.Simple exposing (circle)
import TypedSvg.Types exposing (Paint(..))


{-| Messages.
-}
type Msg
    = MsgMovedTo P2
    | MsgInternal InternalMsg


{-| Model.
-}
type alias Model =
    { location : P2
    , style : Style
    , internal : InternalModel
    }


{-| Style.
-}
type Style
    = Style
        { radius : Float
        , strokeWidth : Float
        , colorStroke : Color
        , colorFillNeutral : Color
        , colorFillMouseOver : Color
        , colorFillDragged : Color
        }


{-| Create an initial model.
-}
init : Style -> P2 -> Model
init style location =
    { style = style
    , location = location
    , internal =
        InternalModel
            { state = StateNeutral
            }
    }


{-| Default style.
-}
defaultStyle : Style
defaultStyle =
    Style
        { radius = 6
        , strokeWidth = 1
        , colorStroke = Color.black
        , colorFillNeutral = Color.lightGray
        , colorFillMouseOver = Color.lightGreen
        , colorFillDragged = Color.lightOrange
        }


type InternalMsg
    = InternalMsgMouseEnter
    | InternalMsgMouseLeave
    | InternalMsgStartDrag
    | InternalMsgStopDrag
    | InternalMsgNoop


type InternalModel
    = InternalModel
        { state : State
        }


type State
    = StateNeutral
    | StateMouseOver
    | StateDragging


{-| Update function.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


{-| Update the model only.
-}
updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        MsgMovedTo p ->
            { model | location = p }

        MsgInternal internalMsg ->
            { model
                | internal =
                    updateInternal internalMsg model model.internal
            }


updateInternal : InternalMsg -> Model -> InternalModel -> InternalModel
updateInternal internalMsg model (InternalModel iModel) =
    case internalMsg of
        InternalMsgMouseEnter ->
            InternalModel { iModel | state = StateMouseOver }

        InternalMsgMouseLeave ->
            InternalModel { iModel | state = StateNeutral }

        InternalMsgStartDrag ->
            InternalModel { iModel | state = StateDragging }

        InternalMsgStopDrag ->
            InternalModel { iModel | state = StateMouseOver }

        InternalMsgNoop ->
            InternalModel iModel


{-| View function.
-}
view : Model -> Drawing Msg
view model =
    let
        (Style style) =
            model.style

        (InternalModel iModel) =
            model.internal

        fillColor =
            case iModel.state of
                StateNeutral ->
                    style.colorFillNeutral

                StateMouseOver ->
                    style.colorFillMouseOver

                StateDragging ->
                    style.colorFillDragged

        appendEvents : Drawing Msg -> Drawing Msg
        appendEvents dwg =
            case iModel.state of
                StateNeutral ->
                    dwg
                        |> T.onMouseEnter
                            (\minfo ->
                                if
                                    minfo.buttons.button1
                                        == T.MouseButtonNotPressed
                                then
                                    MsgInternal InternalMsgMouseEnter

                                else
                                    MsgInternal InternalMsgNoop
                            )

                StateMouseOver ->
                    dwg
                        |> T.onMouseLeave
                            (\_ -> MsgInternal InternalMsgMouseLeave)
                        |> T.onMouseDown
                            (\minfo ->
                                if
                                    minfo.buttons.button1
                                        == T.MouseButtonPressed
                                then
                                    MsgInternal <| InternalMsgStartDrag

                                else
                                    MsgInternal InternalMsgNoop
                            )

                StateDragging ->
                    dwg
                        |> T.onMouseUp (\_ -> MsgInternal InternalMsgStopDrag)
                        |> T.onHostMouseMove
                            (\minfo ->
                                let
                                    delta =
                                        Math.v2Sub
                                            (Math.p2v minfo.localPoint)
                                            (Math.p2v model.location)

                                    newPt =
                                        Math.v2Add
                                            delta
                                            (Math.p2v model.location)
                                in
                                MsgMovedTo (Math.v2p newPt)
                            )
    in
    T.group
        [ T.path
            (circle
                { r = style.radius
                , cx = Math.p2x model.location
                , cy = Math.p2y model.location
                }
            )
            |> T.strokeWidth style.strokeWidth
            |> T.stroke (Paint style.colorStroke)
            |> T.fill (Paint fillColor)
            |> appendEvents
        , T.dropAnchor "location"
            ( Math.p2x model.location
            , Math.p2y model.location
            )
        ]
