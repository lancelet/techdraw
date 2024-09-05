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
import Techdraw.Internal.CSysDict as CSysDict exposing (CSysDict)
import Techdraw.Internal.Svg.Env exposing (EventHandlerCapturedEnv(..))
import Techdraw.Math as Math exposing (AffineTransform, P2)
import TypedSvg.Core exposing (Attribute)


{-| Convert a list of event handlers to a list of SVG attributes.
-}
eventHandlersToSvgAttrs :
    CSysDict
    -> List (EventHandlerCapturedEnv msg)
    -> List (Attribute msg)
eventHandlersToSvgAttrs cSysDict =
    List.map (eventHandlerToSvgAttr cSysDict)


{-| Convert an `EventHandler msg` to an `Attribute msg` using appropriate
environmental information.
-}
eventHandlerToSvgAttr :
    CSysDict
    -> EventHandlerCapturedEnv msg
    -> Attribute msg
eventHandlerToSvgAttr cSysDict eventHandlerWithCapturedEnv =
    let
        (EventHandlerCapturedEnv ecap) =
            eventHandlerWithCapturedEnv

        processMouseHandler : String -> MouseHandler msg -> Attribute msg
        processMouseHandler name =
            mouseHandlerToSvgAttr name ecap.localToWorld cSysDict
    in
    case ecap.eventHandler of
        MouseClick mouseHandler ->
            processMouseHandler "click" mouseHandler

        MouseContextMenu mouseHandler ->
            processMouseHandler "contextmenu" mouseHandler

        MouseDblClick mouseHandler ->
            processMouseHandler "dblclick" mouseHandler

        MouseDown mouseHandler ->
            processMouseHandler "mousedown" mouseHandler

        MouseEnter mouseHandler ->
            processMouseHandler "mouseenter" mouseHandler

        MouseLeave mouseHandler ->
            processMouseHandler "mouseleave" mouseHandler

        MouseMove mouseHandler ->
            processMouseHandler "mousemove" mouseHandler

        MouseOut mouseHandler ->
            processMouseHandler "mouseout" mouseHandler

        MouseOver mouseHandler ->
            processMouseHandler "mouseover" mouseHandler

        MouseUp mouseHandler ->
            processMouseHandler "mouseup" mouseHandler


{-| Convert a `MouseHandler msg` to an `Attribute msg` using appropriate
environmental information.
-}
mouseHandlerToSvgAttr :
    String
    -> AffineTransform
    -> CSysDict
    -> MouseHandler msg
    -> Attribute msg
mouseHandlerToSvgAttr eventName localToWorld cSysDict mouseHandler =
    HtmlEvents.on
        eventName
        (mouseHandlerToMessageDecoder localToWorld cSysDict mouseHandler)



---- JSON Decoders ------------------------------------------------------------


{-| Create a `Decoder msg` from a `MouseHandler msg` and appropriate
environmental information.

TODO: This function will require information about the coordinate systems to
create the pointIn function.

-}
mouseHandlerToMessageDecoder :
    AffineTransform
    -> CSysDict
    -> MouseHandler msg
    -> Decoder msg
mouseHandlerToMessageDecoder localToWorld cSysDict (MouseHandler toMsg) =
    decodeMouseInfo localToWorld cSysDict |> D.map toMsg


{-| Decode mouse information for an event handler.
-}
decodeMouseInfo : AffineTransform -> CSysDict -> Decoder MouseInfo
decodeMouseInfo localToWorld cSysDict =
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
                , pointIn = \name -> CSysDict.inCSys cSysDict name offsetP2
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
