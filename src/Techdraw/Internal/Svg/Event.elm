module Techdraw.Internal.Svg.Event exposing (eventHandlersToSvgAttrs)

{-| Conversion of events to SVG.

@docs eventHandlersToSvgAttrs

-}

import Bitwise exposing (and, shiftLeftBy)
import Html.Events as HtmlEvents
import Json.Decode as D exposing (Decoder)
import Techdraw.Event
    exposing
        ( BState(..)
        , Buttons
        , EventHandler(..)
        , KState(..)
        , Modifiers
        , MouseHandler(..)
        , MouseInfo(..)
        )
import Techdraw.Internal.Svg.Env as Env exposing (Env)
import Techdraw.Math as Math exposing (AffineTransform, P2)
import TypedSvg.Core exposing (Attribute)


{-| Convert a list of event handlers to a list of SVG attributes.
-}
eventHandlersToSvgAttrs :
    Env msg
    -> List (EventHandler msg)
    -> List (Attribute msg)
eventHandlersToSvgAttrs env =
    List.map (eventHandlerToSvgAttr (Env.getLocalToWorld env))


{-| Convert an `EventHandler msg` to an `Attribute msg` using appropriate
environmental information.
-}
eventHandlerToSvgAttr :
    AffineTransform
    -> EventHandler msg
    -> Attribute msg
eventHandlerToSvgAttr localToWorld eventHandler =
    case eventHandler of
        MouseClick mouseHandler ->
            mouseHandlerToSvgAttr "click" localToWorld mouseHandler

        MouseContextMenu mouseHandler ->
            mouseHandlerToSvgAttr "contextmenu" localToWorld mouseHandler

        MouseDblClick mouseHandler ->
            mouseHandlerToSvgAttr "dblclick" localToWorld mouseHandler

        MouseDown mouseHandler ->
            mouseHandlerToSvgAttr "mousedown" localToWorld mouseHandler

        MouseEnter mouseHandler ->
            mouseHandlerToSvgAttr "mouseenter" localToWorld mouseHandler

        MouseLeave mouseHandler ->
            mouseHandlerToSvgAttr "mouseleave" localToWorld mouseHandler

        MouseMove mouseHandler ->
            mouseHandlerToSvgAttr "mousemove" localToWorld mouseHandler

        MouseOut mouseHandler ->
            mouseHandlerToSvgAttr "mouseout" localToWorld mouseHandler

        MouseOver mouseHandler ->
            mouseHandlerToSvgAttr "mouseover" localToWorld mouseHandler

        MouseUp mouseHandler ->
            mouseHandlerToSvgAttr "mouseup" localToWorld mouseHandler


{-| Convert a `MouseHandler msg` to an `Attribute msg` using appropriate
environmental information.
-}
mouseHandlerToSvgAttr :
    String
    -> AffineTransform
    -> MouseHandler msg
    -> Attribute msg
mouseHandlerToSvgAttr eventName localToWorld mouseHandler =
    HtmlEvents.on
        eventName
        (mouseHandlerToMessageDecoder localToWorld mouseHandler)



---- JSON Decoders ------------------------------------------------------------


{-| Create a `Decoder msg` from a `MouseHandler msg` and appropriate
environmental information.

TODO: This function will require information about the coordinate systems to
create the pointIn function.

-}
mouseHandlerToMessageDecoder :
    AffineTransform
    -> MouseHandler msg
    -> Decoder msg
mouseHandlerToMessageDecoder localToWorld (MouseHandler mouseInfoToMsg) =
    decodeMouseInfo localToWorld |> D.map mouseInfoToMsg


{-| Decode mouse information for an event handler.

TODO: This function will require information about the coordinate systems to
create the pointIn function.

-}
decodeMouseInfo : AffineTransform -> Decoder MouseInfo
decodeMouseInfo localToWorld =
    D.map3
        (\offsetP2 buttons modifiers ->
            MouseInfo
                { offset = offsetP2
                , local =
                    Math.p2ApplyAffineTransform
                        (Math.affInvert localToWorld)
                        offsetP2
                , buttons = buttons
                , modifiers = modifiers
                , pointIn = \_ -> Math.p2 0 0 -- TODO: Use CSys lookup
                }
        )
        decodeOffsetP2
        decodeButtons
        decodeModifiers


decodeOffsetP2 : Decoder P2
decodeOffsetP2 =
    D.map2 Math.p2 decodeOffsetX decodeOffsetY


decodeOffsetX : Decoder Float
decodeOffsetX =
    D.field "offsetX" D.float


decodeOffsetY : Decoder Float
decodeOffsetY =
    D.field "offsetY" D.float


decodeButtons : Decoder Buttons
decodeButtons =
    D.field "buttons" D.int |> D.map intToButtons


intToButtons : Int -> Buttons
intToButtons i =
    { b1 = getBitForBState 0 i
    , b2 = getBitForBState 1 i
    , b3 = getBitForBState 2 i
    , b4 = getBitForBState 3 i
    , b5 = getBitForBState 4 i
    }


getBitForBState : Int -> Int -> BState
getBitForBState input bitNumber =
    if and input (shiftLeftBy bitNumber 0x01) == 0 then
        BUp

    else
        BDown


decodeModifiers : Decoder Modifiers
decodeModifiers =
    D.map4
        (\ctrl shift alt meta ->
            { ctrl = ctrl, shift = shift, alt = alt, meta = meta }
        )
        decodeCtrlKey
        decodeShiftKey
        decodeAltKey
        decodeMetaKey


decodeCtrlKey : Decoder KState
decodeCtrlKey =
    D.field "ctrlKey" D.bool |> D.map boolToKState


decodeShiftKey : Decoder KState
decodeShiftKey =
    D.field "shiftKey" D.bool |> D.map boolToKState


decodeAltKey : Decoder KState
decodeAltKey =
    D.field "altKey" D.bool |> D.map boolToKState


decodeMetaKey : Decoder KState
decodeMetaKey =
    D.field "metaKey" D.bool |> D.map boolToKState


boolToKState : Bool -> KState
boolToKState b =
    if b then
        KDown

    else
        KUp
