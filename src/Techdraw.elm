module Techdraw exposing
    ( Drawing
    , ViewBox
    , render, renderSvgElement
    , empty, path, svg, group, tagCSys, transform
    , translate, rotateAbout, skewX
    , style
    , fill, stroke, strokeWidth
    , onClick, onContextMenu, onDblClick, onMouseDown
    , onMouseEnter, onMouseLeave, onMouseMove
    , onMouseOut, onMouseOver, onMouseUp
    , Style
    , styleDefault, styleInheritAll
    , styleSetFill, styleSetStroke, styleSetStrokeWidth
    , styleGetFill, styleGetStroke, styleGetStrokeWidth
    , styleAppendDecorator, styleAppendAttribute
    , styleGetDecorators, styleGetAttributes
    , Decorator(..)
    , MouseInfo, ModifierKeys, MouseButtons
    , MouseButtonState(..), KeyPressState(..)
    , CSysName(..)
    )

{-|


# Drawing


## Types

@docs Drawing
@docs ViewBox


## Creating SVG

@docs render, renderSvgElement


## Creating Drawings


### First-class Operations

@docs empty, path, svg, group, tagCSys, transform


### Derived Operations

@docs translate, rotateAbout, skewX


## Styling Drawings

@docs style
@docs fill, stroke, strokeWidth


## Handling Events

@docs onClick, onContextMenu, onDblClick, onMouseDown
@docs onMouseEnter, onMouseLeave, onMouseMove
@docs onMouseOut, onMouseOver, onMouseUp


# Styles

@docs Style
@docs styleDefault, styleInheritAll
@docs styleSetFill, styleSetStroke, styleSetStrokeWidth
@docs styleGetFill, styleGetStroke, styleGetStrokeWidth
@docs styleAppendDecorator, styleAppendAttribute
@docs styleGetDecorators, styleGetAttributes


# Path Decoration

@docs Decorator


# Event Information

@docs MouseInfo, ModifierKeys, MouseButtons
@docs MouseButtonState, KeyPressState


# Coordinate Systems

@docs CSysName

-}

import Bitwise
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events as HtmlEvents
import Json.Decode as Decode exposing (Decoder, bool)
import Techdraw.Math as Math exposing (AffineTransform(..), P2, Path)
import TypedSvg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Display(..), Paint, px)
import VirtualDom exposing (Attribute)



---- Drawing ------------------------------------------------------------------


{-| Drawing.
-}
type Drawing msg
    = DrawingEmpty
    | DrawingPath Path
    | DrawingSvg (Style msg -> AffineTransform -> Svg msg)
    | DrawingStyled (Style msg) (Drawing msg)
    | DrawingTransformed AffineTransform (Drawing msg)
    | DrawingGroup (List (Drawing msg))
    | DrawingEvents (Events msg) (Drawing msg)
    | DrawingTagCSys CSysName (Drawing msg)


{-| Empty drawing.
-}
empty : Drawing msg
empty =
    DrawingEmpty


{-| Draw a path.
-}
path : Path -> Drawing msg
path =
    DrawingPath


{-| Draw some SVG directly.

The function provided takes the current style and local-to-world
transformation so that it can produce custom SVG.

-}
svg : (Style msg -> AffineTransform -> Svg msg) -> Drawing msg
svg =
    DrawingSvg


{-| Transform a drawing.
-}
transform : AffineTransform -> Drawing msg -> Drawing msg
transform parentTransform drawing =
    case drawing of
        DrawingTransformed childTransform childDrawing ->
            DrawingTransformed
                (Math.affMul parentTransform childTransform)
                childDrawing

        _ ->
            DrawingTransformed parentTransform drawing


{-| Translate a drawing.
-}
translate : ( Float, Float ) -> Drawing msg -> Drawing msg
translate ( tx, ty ) =
    let
        xform =
            Math.affTranslate <| Math.Translation tx ty
    in
    transform xform


{-| Rotate clockwise by a value in degrees about a point.
-}
rotateAbout : Float -> ( Float, Float ) -> Drawing msg -> Drawing msg
rotateAbout angle ( xc, yc ) =
    let
        xform =
            Math.affFromComponents
                [ Math.ComponentTranslation <| Math.Translation -xc -yc
                , Math.ComponentRotation <|
                    Math.Rotation <|
                        Math.angle2Pi (angle * pi / 180)
                , Math.ComponentTranslation <| Math.Translation xc yc
                ]
    in
    transform xform


{-| Skew along the x-axis by a value in degrees.
-}
skewX : Float -> Drawing msg -> Drawing msg
skewX angle =
    let
        xform =
            Math.affShearX <| Math.ShearX <| tan <| angle * pi / 180
    in
    transform xform


{-| Group drawings.
-}
group : List (Drawing msg) -> Drawing msg
group =
    DrawingGroup


{-| Tag a coordinate system.

This tags the coordinate system of a particular drawing with the name supplied.
All coordinate systems that are part of a drawing are made available to
mouse events that are later registered for that drawing.

-}
tagCSys : CSysName -> Drawing msg -> Drawing msg
tagCSys name =
    DrawingTagCSys name


{-| Set the style of a drawing.

The style is applied "on the outside". Any styles already defined will take
precedence.

-}
style : Style msg -> Drawing msg -> Drawing msg
style parentStyle drawing =
    case drawing of
        DrawingStyled childStyle dwg ->
            -- Any existing child style applied to the drawing takes
            -- precedence.
            DrawingStyled (combineStyles parentStyle childStyle) dwg

        _ ->
            DrawingStyled parentStyle drawing


{-| Set the fill.

See [`style`](#style) for information on precedence.

-}
fill : Paint -> Drawing msg -> Drawing msg
fill paint drawing =
    style (styleDefault |> styleSetFill paint) drawing


{-| Set the stroke.

See [`style`](#style) for information on precedence.

-}
stroke : Paint -> Drawing msg -> Drawing msg
stroke paint drawing =
    style (styleDefault |> styleSetStroke paint) drawing


{-| Set the stroke width.

See [`style`](#style) for information on precedence.

-}
strokeWidth : Float -> Drawing msg -> Drawing msg
strokeWidth width drawing =
    style (styleDefault |> styleSetStrokeWidth width) drawing



---- Styles -------------------------------------------------------------------


{-| Indicate whether a style setting is inherited or explicitly set.
-}
type StyleSetting a
    = Inherited
    | Set a


{-| Convert a `StyleSetting` to a `Maybe` value.
-}
styleSettingToMaybe : StyleSetting a -> Maybe a
styleSettingToMaybe setting =
    case setting of
        Inherited ->
            Nothing

        Set value ->
            Just value


{-| Combine a style setting from a parent and child into a new current style
setting.
-}
combineStyleSetting : StyleSetting a -> StyleSetting a -> StyleSetting a
combineStyleSetting parent child =
    case ( parent, child ) of
        ( _, Set value ) ->
            Set value

        ( Set value, Inherited ) ->
            Set value

        ( Inherited, Inherited ) ->
            Inherited


{-| Style.
-}
type Style msg
    = Style
        { fill : StyleSetting Paint
        , stroke : StyleSetting Paint
        , strokeWidth : StyleSetting Float
        , decorators : List (Decorator msg)
        , extraAttributes : List (Attribute msg)
        }


{-| Default style.

This is a synonym for [`styleInheritAll`](#styleInheritAll), which inherits
everything from its parent.

-}
styleDefault : Style msg
styleDefault =
    styleInheritAll


{-| A style which inherits everything from its parent.
-}
styleInheritAll : Style msg
styleInheritAll =
    Style
        { fill = Inherited
        , stroke = Inherited
        , strokeWidth = Inherited
        , decorators = []
        , extraAttributes = []
        }


{-| Set the fill.
-}
styleSetFill : Paint -> Style msg -> Style msg
styleSetFill paint (Style styl) =
    Style <| { styl | fill = Set paint }


{-| Return the fill setting.
-}
styleGetFill : Style msg -> Maybe Paint
styleGetFill (Style styl) =
    styl.fill |> styleSettingToMaybe


{-| Set the stroke.
-}
styleSetStroke : Paint -> Style msg -> Style msg
styleSetStroke paint (Style styl) =
    Style <| { styl | stroke = Set paint }


{-| Return the stroke setting.
-}
styleGetStroke : Style msg -> Maybe Paint
styleGetStroke (Style styl) =
    styl.stroke |> styleSettingToMaybe


{-| Set the stroke width.
-}
styleSetStrokeWidth : Float -> Style msg -> Style msg
styleSetStrokeWidth width (Style styl) =
    Style <| { styl | strokeWidth = Set width }


{-| Return the stroke width.
-}
styleGetStrokeWidth : Style msg -> Maybe Float
styleGetStrokeWidth (Style styl) =
    styl.strokeWidth |> styleSettingToMaybe


{-| Append a decorator.
-}
styleAppendDecorator : Decorator msg -> Style msg -> Style msg
styleAppendDecorator decorator (Style styl) =
    Style <| { styl | decorators = decorator :: styl.decorators }


{-| Return the decorators.

The decorators are returned in the sequence they should be applied.

-}
styleGetDecorators : Style msg -> List (Decorator msg)
styleGetDecorators (Style styl) =
    styl.decorators |> List.reverse


{-| Append a custom SVG attribute.

Typically this is used for setting styling attributes that are not directly
supported by the library. However, any provided attribute will be appended
to the output SVG.

-}
styleAppendAttribute : Attribute msg -> Style msg -> Style msg
styleAppendAttribute attribute (Style styl) =
    Style <| { styl | extraAttributes = attribute :: styl.extraAttributes }


{-| Return the attributes.

The attributes are returned in the sequence they should be applied.

-}
styleGetAttributes : Style msg -> List (Attribute msg)
styleGetAttributes (Style styl) =
    styl.extraAttributes |> List.reverse


{-| Combine settings from a parent style and child style into a new
current style.
-}
combineStyles : Style msg -> Style msg -> Style msg
combineStyles (Style parent) (Style child) =
    let
        comb extract =
            combineStyleSetting (extract parent) (extract child)
    in
    Style
        { fill = comb .fill
        , stroke = comb .stroke
        , strokeWidth = comb .strokeWidth
        , decorators = child.decorators ++ parent.decorators
        , extraAttributes = child.extraAttributes ++ parent.extraAttributes
        }



---- Path Decoration ----------------------------------------------------------


{-| A decorator is a special styling operation that can operate on a
processed path.

A decorator runs on a path after:

  - A collapsed style has been computed (taking into account the styles
    of groups above the path, etc)
  - The local-to-world transformation has been computed.
  - The path has been transformed to world space.

A `Decorator` can then modify the path as it sees fit, to produce a new
[`Drawing`](#Drawing). The original `Drawing` is discarded, so if the
aim is to retain it, then it must be part of the `Drawing` returned by
the `Decorator`.

TODO: There should probably be a different layout for decorators. For
example, we want to support decorator operations like this:

  - Shorten a path, then add an arrow. This requires a sequencing of
    secorators (shorten, THEN add arrow geometry). Currently, there's
    no way to do sequencing.

-}
type Decorator msg
    = Decorator
        (Style msg
         -> AffineTransform
         -> Path
         -> Drawing msg
        )



---- Events -------------------------------------------------------------------


{-| Register an `onclick` listener.
-}
onClick : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onClick fn =
    fn |> MouseHandler |> MouseClick |> registerListener


{-| Register an `oncontextmenu` listener.
-}
onContextMenu : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onContextMenu fn =
    fn |> MouseHandler |> MouseContextMenu |> registerListener


{-| Register an `ondblclick` listener.
-}
onDblClick : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onDblClick fn =
    fn |> MouseHandler |> MouseDblClick |> registerListener


{-| Register an `onmousedown` listener.
-}
onMouseDown : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseDown fn =
    fn |> MouseHandler |> MouseDown |> registerListener


{-| Register an `onmouseenter` listener.
-}
onMouseEnter : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseEnter fn =
    fn |> MouseHandler |> MouseEnter |> registerListener


{-| Register an `onmouseleave` listener.
-}
onMouseLeave : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseLeave fn =
    fn |> MouseHandler |> MouseLeave |> registerListener


{-| Register an `onmousemove` listener.
-}
onMouseMove : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseMove fn =
    fn |> MouseHandler |> MouseMove |> registerListener


{-| Register an `onmouseout` listener.
-}
onMouseOut : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseOut fn =
    fn |> MouseHandler |> MouseOut |> registerListener


{-| Register an `onmouseover` listener.
-}
onMouseOver : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseOver fn =
    fn |> MouseHandler |> MouseOver |> registerListener


{-| Register an `onmouseup` listener.
-}
onMouseUp : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseUp fn =
    fn |> MouseHandler |> MouseUp |> registerListener


{-| Register an event listener.
-}
registerListener : EventListener msg -> Drawing msg -> Drawing msg
registerListener listener drawing =
    case drawing of
        DrawingEvents events childDrawing ->
            DrawingEvents (eventsAddListener listener events) childDrawing

        _ ->
            DrawingEvents (newEventsWithListener listener) drawing


{-| Events.
-}
type Events msg
    = Events (List (EventListener msg))


{-| Create an empty `Events`.
-}
emptyEvents : Events msg
emptyEvents =
    Events []


{-| Create a new `Events` containing a single listener.
-}
newEventsWithListener : EventListener msg -> Events msg
newEventsWithListener listener =
    Events [ listener ]


{-| Add an event listener.
-}
eventsAddListener : EventListener msg -> Events msg -> Events msg
eventsAddListener listener (Events es) =
    Events <| listener :: es


{-| Combine events listeners.
-}
combineEvents : Events msg -> Events msg -> Events msg
combineEvents (Events parentList) (Events childList) =
    Events (parentList ++ childList)


{-| Types of event listeners and their handlers.
-}
type EventListener msg
    = MouseClick (MouseHandler msg)
    | MouseContextMenu (MouseHandler msg)
    | MouseDblClick (MouseHandler msg)
    | MouseDown (MouseHandler msg)
    | MouseEnter (MouseHandler msg)
    | MouseLeave (MouseHandler msg)
    | MouseMove (MouseHandler msg)
    | MouseOut (MouseHandler msg)
    | MouseOver (MouseHandler msg)
    | MouseUp (MouseHandler msg)


{-| Information about a mouse event.

The fields of this record are as follows:

  - `clientPoint`: The `(clientX, clientY)` position of the mouse event.
  - `localPoint`: The point in the local coordinate system.
  - `buttons`: Which buttons were pressed.
  - `modifiers`: What modifier keys were pressed.
  - `pointIn`: A function to fetch the point in a named coordinate system.

-}
type alias MouseInfo =
    { clientPoint : P2
    , localPoint : P2
    , buttons : MouseButtons
    , modifiers : ModifierKeys
    , pointIn : CSysName -> P2
    }


{-| Indicates whether a mouse button was pressed or not pressed.
-}
type MouseButtonState
    = MouseButtonPressed
    | MouseButtonNotPressed


{-| Mouse buttons.
-}
type alias MouseButtons =
    { button1 : MouseButtonState
    , button2 : MouseButtonState
    , button3 : MouseButtonState
    , button4 : MouseButtonState
    , button5 : MouseButtonState
    }


{-| Indicates whether a key was pressed or not pressed.
-}
type KeyPressState
    = KeyPressed
    | KeyNotPressed


{-| Modifier key state.
-}
type alias ModifierKeys =
    { ctrl : KeyPressState
    , shift : KeyPressState
    , alt : KeyPressState
    , meta : KeyPressState
    }


{-| A MouseHandler event.

It receieves the local-to-world transform and world mouse coordinates.

-}
type MouseHandler msg
    = MouseHandler (MouseInfo -> msg)


{-| Convert listed event handlers to attributes.
-}
eventsToAttributes :
    AffineTransform
    -> CSysDict
    -> Events msg
    -> List (Attribute msg)
eventsToAttributes localToWorld cSysDict (Events es) =
    List.map (eventListenerToAttribute localToWorld cSysDict) es


eventListenerToAttribute :
    AffineTransform
    -> CSysDict
    -> EventListener msg
    -> Attribute msg
eventListenerToAttribute localToWorld cSysDict listener =
    let
        mouseA name handler =
            mouseHandlerToAttribute localToWorld cSysDict name handler
    in
    case listener of
        MouseClick handler ->
            mouseA "click" handler

        MouseContextMenu handler ->
            mouseA "contextmenu" handler

        MouseDblClick handler ->
            mouseA "dblclick" handler

        MouseDown handler ->
            mouseA "mousedown" handler

        MouseEnter handler ->
            mouseA "mouseenter" handler

        MouseLeave handler ->
            mouseA "mouseleave" handler

        MouseMove handler ->
            mouseA "mousemove" handler

        MouseOut handler ->
            mouseA "mouseout" handler

        MouseOver handler ->
            mouseA "mouseover" handler

        MouseUp handler ->
            mouseA "mouseup" handler


mouseHandlerToAttribute :
    AffineTransform
    -> CSysDict
    -> String
    -> MouseHandler msg
    -> Attribute msg
mouseHandlerToAttribute localToWorld cSysDict eventName mouseHandler =
    HtmlEvents.on
        eventName
        (mouseHandlerDecoder localToWorld cSysDict mouseHandler)


mouseHandlerDecoder :
    AffineTransform
    -> CSysDict
    -> MouseHandler msg
    -> Decoder msg
mouseHandlerDecoder localToWorld cSysDict (MouseHandler mkMsg) =
    Decode.map mkMsg (mouseInfo localToWorld cSysDict)


mouseInfo : AffineTransform -> CSysDict -> Decoder MouseInfo
mouseInfo localToWorld cSysDict =
    let
        calcLocalPoint : P2 -> P2
        calcLocalPoint clientPoint =
            Math.affInvert localToWorld
                |> (\mat -> Math.affApplyP2 mat clientPoint)

        calcPointIn : CSysName -> P2 -> P2
        calcPointIn name clientPoint =
            getCSys name cSysDict
                |> Maybe.withDefault localToWorld
                |> Math.affInvert
                |> (\mat -> Math.affApplyP2 mat clientPoint)
    in
    Decode.map3
        (\clientPoint btns mods ->
            { clientPoint = clientPoint
            , localPoint = calcLocalPoint clientPoint
            , buttons = btns
            , modifiers = mods
            , pointIn = \name -> calcPointIn name clientPoint
            }
        )
        clientP2
        mouseButtons
        modifiers


clientP2 : Decoder P2
clientP2 =
    Decode.map2 Math.p2 clientXFloat clientYFloat


clientXFloat : Decoder Float
clientXFloat =
    Decode.map toFloat clientX


clientYFloat : Decoder Float
clientYFloat =
    Decode.map toFloat clientY


mouseButtons : Decoder MouseButtons
mouseButtons =
    let
        buttonState : Int -> Int -> MouseButtonState
        buttonState buttonNumber input =
            if
                Bitwise.and
                    input
                    (Bitwise.shiftLeftBy buttonNumber 0x01)
                    == 0
            then
                MouseButtonNotPressed

            else
                MouseButtonPressed
    in
    Decode.map
        (\b ->
            { button1 = buttonState 1 b
            , button2 = buttonState 2 b
            , button3 = buttonState 3 b
            , button4 = buttonState 4 b
            , button5 = buttonState 5 b
            }
        )
        buttons


modifiers : Decoder ModifierKeys
modifiers =
    let
        boolToKeyPressState : Bool -> KeyPressState
        boolToKeyPressState boolValue =
            if boolValue then
                KeyPressed

            else
                KeyNotPressed
    in
    Decode.map4
        (\ctrl shift alt meta ->
            { ctrl = ctrl |> boolToKeyPressState
            , shift = shift |> boolToKeyPressState
            , alt = alt |> boolToKeyPressState
            , meta = meta |> boolToKeyPressState
            }
        )
        ctrlKey
        shiftKey
        altKey
        metaKey


clientX : Decoder Int
clientX =
    Decode.field "clientX" Decode.int


clientY : Decoder Int
clientY =
    Decode.field "clientY" Decode.int


buttons : Decoder Int
buttons =
    Decode.field "buttons" Decode.int


ctrlKey : Decoder Bool
ctrlKey =
    Decode.field "ctrlKey" Decode.bool


shiftKey : Decoder Bool
shiftKey =
    Decode.field "shiftKey" Decode.bool


altKey : Decoder Bool
altKey =
    Decode.field "altKey" Decode.bool


metaKey : Decoder Bool
metaKey =
    Decode.field "metaKey" Decode.bool



---- Rendering ----------------------------------------------------------------


{-| Viewbox for a drawing.

This should typiecally be the same viewbox that SVG uses.

The default coordinate system of a `Drawing` has the origin at the bottom
left. The x-axis increases to the right, and the y-axis increases upwards.

-}
type alias ViewBox =
    { minX : Float
    , minY : Float
    , width : Float
    , height : Float
    }


{-| Render a diagram to a stand-alone SVG image.

This creates an SVG element whose pixel size is the same as the
height and width of the `ViewBox`.

-}
render : ViewBox -> Drawing msg -> Html msg
render viewBox drawing =
    TypedSvg.svg
        [ SvgAttributes.width (px viewBox.width)
        , SvgAttributes.height (px viewBox.height)
        , SvgAttributes.viewBox
            viewBox.minX
            viewBox.minY
            viewBox.width
            viewBox.height
        ]
        [ renderSvgElement viewBox drawing
        ]


{-| Render a diagram to an SVG element.
-}
renderSvgElement : ViewBox -> Drawing msg -> Svg msg
renderSvgElement viewBox =
    renderWithState (initState viewBox)


type State msg
    = State
        { style : Style msg
        , events : Events msg
        , localToWorld : AffineTransform
        , viewBox : ViewBox
        , coordinateSystems : CSysDict
        }


initState : ViewBox -> State msg
initState viewBox =
    let
        localToWorld =
            Math.affFromComponents
                [ Math.ComponentScale <| Math.Scale 1 -1
                , Math.ComponentTranslation <|
                    Math.Translation 0 viewBox.height
                , Math.ComponentTranslation <|
                    Math.Translation viewBox.minX viewBox.minY
                ]
    in
    State
        { style = styleDefault
        , events = emptyEvents
        , localToWorld = localToWorld
        , viewBox = viewBox
        , coordinateSystems = emptyCSysDict
        }


stateStyle : State msg -> Style msg
stateStyle (State state) =
    state.style


stateEvents : State msg -> Events msg
stateEvents (State state) =
    state.events


stateLocalToWorld : State msg -> AffineTransform
stateLocalToWorld (State state) =
    state.localToWorld


stateViewBox : State msg -> ViewBox
stateViewBox (State state) =
    state.viewBox


stateCoordinateSystems : State msg -> CSysDict
stateCoordinateSystems (State state) =
    state.coordinateSystems


stateCombineStyles : State msg -> Style msg -> State msg
stateCombineStyles (State parent) child =
    State { parent | style = combineStyles parent.style child }


stateCombineEvents : State msg -> Events msg -> State msg
stateCombineEvents (State parent) child =
    State { parent | events = combineEvents parent.events child }


stateComposeTransform : State msg -> AffineTransform -> State msg
stateComposeTransform (State parent) child =
    State { parent | localToWorld = Math.affMul parent.localToWorld child }


stateInsertCoordinateSystem :
    ( CSysName, AffineTransform )
    -> State msg
    -> State msg
stateInsertCoordinateSystem pair (State parent) =
    State
        { parent
            | coordinateSystems =
                insertCSys pair parent.coordinateSystems
        }


{-| Render a diagram as SVG.

This is a recursive evaluator for now. If necessary, it can be converted to
use something like a CEK machine.

-}
renderWithState : State msg -> Drawing msg -> Svg msg
renderWithState state drawing =
    let
        styl : Style msg
        styl =
            state |> stateStyle

        events : Events msg
        events =
            state |> stateEvents

        localToWorld : AffineTransform
        localToWorld =
            state |> stateLocalToWorld

        decorators : List (Decorator msg)
        decorators =
            styl |> styleGetDecorators
    in
    case drawing of
        DrawingEmpty ->
            TypedSvg.g [] []

        DrawingPath pth ->
            if List.isEmpty decorators then
                -- The style has no decorators. This means the path has no
                -- further processing required and will be dumped to SVG
                -- directly.
                case pth of
                    Math.EmptyPath ->
                        -- For an empty path, we dump out an empty group, with
                        -- a `display = "none"` attribute.
                        TypedSvg.g [ SvgAttributes.display DisplayNone ] []

                    Math.Path _ ->
                        let
                            styleAttributes =
                                styleToAttributes styl

                            eventAttributes =
                                eventsToAttributes
                                    localToWorld
                                    (stateCoordinateSystems state)
                                    events

                            pathAttr =
                                Math.affApplyPath localToWorld pth
                                    |> Math.toSvgPPath
                                    |> SvgAttributes.d

                            attributes =
                                pathAttr
                                    :: (styleAttributes
                                            ++ eventAttributes
                                       )
                        in
                        TypedSvg.path attributes []

            else
                -- The style has decorators, so we must process them to
                -- produce a new drawing. At this point, we strip out all
                -- the previous transformations and style, since the
                -- decorator has access to those directly.
                let
                    xfpath =
                        Math.affApplyPath localToWorld pth
                in
                List.map
                    (\(Decorator f) -> f styl localToWorld xfpath)
                    decorators
                    |> group
                    |> renderWithState (initState (stateViewBox state))

        DrawingSvg mkSvgChild ->
            mkSvgChild styl localToWorld

        DrawingStyled parentStyle childDrawing ->
            renderWithState
                (stateCombineStyles state parentStyle)
                childDrawing

        DrawingTransformed childToParent childDrawing ->
            renderWithState
                (stateComposeTransform state childToParent)
                childDrawing

        DrawingGroup children ->
            TypedSvg.g [] <| List.map (renderWithState state) children

        DrawingEvents evts childDrawing ->
            renderWithState (stateCombineEvents state evts) childDrawing

        DrawingTagCSys cSysName childDrawing ->
            renderWithState
                (stateInsertCoordinateSystem ( cSysName, localToWorld ) state)
                childDrawing


{-| Convert a style to a list of SVG attributes.
-}
styleToAttributes : Style msg -> List (Attribute msg)
styleToAttributes styl =
    let
        toAttr :
            (Style msg -> Maybe a)
            -> (a -> Attribute msg)
            -> Maybe (Attribute msg)
        toAttr extract produce =
            extract styl |> Maybe.map produce

        styleAttrs =
            List.filterMap (\x -> x)
                [ toAttr styleGetFill SvgAttributes.fill
                , toAttr styleGetStroke SvgAttributes.stroke
                , toAttr styleGetStrokeWidth
                    (\x -> SvgAttributes.strokeWidth (px x))
                ]
    in
    styleAttrs ++ List.reverse (styleGetAttributes styl)



---- Coordinate systems -------------------------------------------------------


{-| Name of a coordinate system.
-}
type CSysName
    = CSysName String


{-| Dictionary of coordinate systems.
-}
type CSysDict
    = CSysDict (Dict String AffineTransform)


{-| Empty coordinate system dictionary.
-}
emptyCSysDict : CSysDict
emptyCSysDict =
    CSysDict <| Dict.empty


{-| Insert a coordinate system into a dictionary.
-}
insertCSys : ( CSysName, AffineTransform ) -> CSysDict -> CSysDict
insertCSys ( CSysName name, localToWorld ) (CSysDict dict) =
    CSysDict <| Dict.insert name localToWorld dict


{-| Get a coordinate system from a dictionary.
-}
getCSys : CSysName -> CSysDict -> Maybe AffineTransform
getCSys (CSysName name) (CSysDict dict) =
    Dict.get name dict
