module Techdraw exposing
    ( Drawing
    , ViewBox
    , render, renderSvgElement
    , empty, path, svg, group, map, tagCSys, transform
    , prependAnchorNamespace, dropAnchor, weighAnchors
    , translate, rotateAbout, scale, skewX
    , style
    , fill, stroke, strokeWidth, strokeLinecap, strokeLinejoin
    , onClick, onContextMenu, onDblClick, onMouseDown
    , onMouseEnter, onMouseLeave, onMouseMove
    , onMouseOut, onMouseOver, onMouseUp
    , onHostMouseLeave, onHostMouseMove, onHostMouseUp
    , Style
    , styleDefault, styleInheritAll
    , styleSetFill, styleSetStroke, styleSetStrokeWidth
    , styleSetStrokeLinecap, styleSetStrokeLinejoin
    , styleGetFill, styleGetStroke, styleGetStrokeWidth
    , styleGetStrokeLinecap, styleGetStrokeLinejoin
    , styleAppendAttribute, styleGetAttributes
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

@docs empty, path, svg, group, map, tagCSys, transform
@docs prependAnchorNamespace, dropAnchor, weighAnchors


### Derived Operations

@docs translate, rotateAbout, scale, skewX


## Styling Drawings

@docs style
@docs fill, stroke, strokeWidth, strokeLinecap, strokeLinejoin


## Handling Events

@docs onClick, onContextMenu, onDblClick, onMouseDown
@docs onMouseEnter, onMouseLeave, onMouseMove
@docs onMouseOut, onMouseOver, onMouseUp
@docs onHostMouseLeave, onHostMouseMove, onHostMouseUp


# Styles

@docs Style
@docs styleDefault, styleInheritAll
@docs styleSetFill, styleSetStroke, styleSetStrokeWidth
@docs styleSetStrokeLinecap, styleSetStrokeLinejoin
@docs styleGetFill, styleGetStroke, styleGetStrokeWidth
@docs styleGetStrokeLinecap, styleGetStrokeLinejoin
@docs styleAppendAttribute, styleGetAttributes


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
import Json.Decode as Decode exposing (Decoder)
import Techdraw.Math as Math exposing (AffineTransform(..), P2)
import Techdraw.Path as P exposing (Path(..))
import Techdraw.Svg.SvgStringPath as SP
import TypedSvg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core as TypedSvgCore exposing (Svg)
import TypedSvg.Types
    exposing
        ( Align(..)
        , Display(..)
        , MeetOrSlice(..)
        , Paint
        , Scale(..)
        , StrokeLinecap
        , StrokeLinejoin
        , px
        )
import VirtualDom exposing (Attribute, mapAttribute)



---- Drawing ------------------------------------------------------------------


{-| Drawing.
-}
type Drawing msg
    = DrawingEmpty
    | DrawingPath Path
    | DrawingSvg (AffineTransform -> Svg msg)
    | DrawingStyled (Style msg) (Drawing msg)
    | DrawingTransformed AffineTransform (Drawing msg)
    | DrawingGroup (List (Drawing msg))
    | DrawingEvents (Events msg) (Drawing msg)
    | DrawingHostEvents (Events msg) (Drawing msg)
    | DrawingTagCSys CSysName
    | DrawingPrependAnchorNamespace AnchorNamespace (Drawing msg)
    | DrawingDropAnchor AnchorName P2
    | DrawingWeighAnchors ((String -> ( Float, Float )) -> Drawing msg)


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

The function provided takes the local-to-world transformation so that it can
produce custom SVG.

-}
svg : (AffineTransform -> Svg msg) -> Drawing msg
svg =
    DrawingSvg


{-| Transform a drawing.
-}
transform : AffineTransform -> Drawing msg -> Drawing msg
transform parentTransform drawing =
    case drawing of
        DrawingTransformed childTransform childDrawing ->
            DrawingTransformed
                (Math.affMatMul parentTransform childTransform)
                childDrawing

        _ ->
            DrawingTransformed parentTransform drawing


{-| Translate a drawing.
-}
translate : ( Float, Float ) -> Drawing msg -> Drawing msg
translate ( tx, ty ) =
    transform <| Math.affTranslation <| Math.Translation (Math.v2 tx ty)


{-| Scale a drawing.
-}
scale : ( Float, Float ) -> Drawing msg -> Drawing msg
scale ( sx, sy ) =
    transform <| Math.affScaling <| Math.Scaling sx sy


{-| Rotate clockwise by a value in degrees about a point.
-}
rotateAbout : Float -> ( Float, Float ) -> Drawing msg -> Drawing msg
rotateAbout angle ( xc, yc ) =
    let
        xform =
            Math.affFromComponents
                [ Math.AffineTranslation <| Math.Translation (Math.v2 -xc -yc)
                , Math.AffineRotation <|
                    Math.Rotation <|
                        Math.angle2Pi (angle * pi / 180)
                , Math.AffineTranslation <| Math.Translation (Math.v2 xc yc)
                ]
    in
    transform xform


{-| Skew along the x-axis by a value in degrees.
-}
skewX : Float -> Drawing msg -> Drawing msg
skewX angle =
    let
        xform =
            Math.affShearingX <| Math.ShearingX <| tan <| angle * pi / 180
    in
    transform xform


{-| Group drawings.
-}
group : List (Drawing msg) -> Drawing msg
group =
    DrawingGroup


{-| Tag a coordinate system.

A drawing which tags its local coordinate system with a name.

-}
tagCSys : CSysName -> Drawing msg
tagCSys name =
    DrawingTagCSys name


{-| Prepend a namespace to any anchor names in the drawing.
-}
prependAnchorNamespace : String -> Drawing msg -> Drawing msg
prependAnchorNamespace name =
    DrawingPrependAnchorNamespace (anchorNamespace name)


{-| Drop an anchor at the specified location in the drawing.
-}
dropAnchor : String -> ( Float, Float ) -> Drawing msg
dropAnchor name ( locX, locY ) =
    DrawingDropAnchor (anchorName name) (Math.p2 locX locY)


{-| Create a drawing using anchor locations.

`weighAnchors` is called with a generation function, that can generate a
drawing based on previously-defined anchor locations. In turn, that
function will be called with a function that can retreive any previously
dropped anchors.

-}
weighAnchors : ((String -> ( Float, Float )) -> Drawing msg) -> Drawing msg
weighAnchors createFn =
    DrawingWeighAnchors createFn


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


{-| Set the stroke line cap.
-}
strokeLinecap : StrokeLinecap -> Drawing msg -> Drawing msg
strokeLinecap lc drawing =
    style (styleDefault |> styleSetStrokeLinecap lc) drawing


{-| Set the stroke line join.
-}
strokeLinejoin : StrokeLinejoin -> Drawing msg -> Drawing msg
strokeLinejoin lj drawing =
    style (styleDefault |> styleSetStrokeLinejoin lj) drawing



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
        , strokeLinecap : StyleSetting StrokeLinecap
        , strokeLinejoin : StyleSetting StrokeLinejoin
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
        , strokeLinecap = Inherited
        , strokeLinejoin = Inherited
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


{-| Set the stroke line cap.
-}
styleSetStrokeLinecap : StrokeLinecap -> Style msg -> Style msg
styleSetStrokeLinecap lc (Style styl) =
    Style <| { styl | strokeLinecap = Set lc }


{-| Get the stroke line cap.
-}
styleGetStrokeLinecap : Style msg -> Maybe StrokeLinecap
styleGetStrokeLinecap (Style styl) =
    styl.strokeLinecap |> styleSettingToMaybe


{-| Set the stroke line join.
-}
styleSetStrokeLinejoin : StrokeLinejoin -> Style msg -> Style msg
styleSetStrokeLinejoin lj (Style styl) =
    Style <| { styl | strokeLinejoin = Set lj }


{-| Get the stroke line join.
-}
styleGetStrokeLinejoin : Style msg -> Maybe StrokeLinejoin
styleGetStrokeLinejoin (Style styl) =
    styl.strokeLinejoin |> styleSettingToMaybe


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
        , strokeLinecap = comb .strokeLinecap
        , strokeLinejoin = comb .strokeLinejoin
        , extraAttributes = child.extraAttributes ++ parent.extraAttributes
        }



---- Path Decoration ----------------------------------------------------------
{- A decorator is a special styling operation that can operate on a
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
{-
   type Decorator msg
       = Decorator
           (Style msg
            -> AffineTransform
            -> Path
            -> Drawing msg
           )
-}
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


{-| Register an `onmouseleave` listener with the host SVG element.
-}
onHostMouseLeave : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseLeave fn =
    fn |> MouseHandler |> MouseLeave |> registerHostListener


{-| Register an `onmousemove` listener with the host SVG element.
-}
onHostMouseMove : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseMove fn =
    fn |> MouseHandler |> MouseMove |> registerHostListener


{-| Register an `onmouseup` listener with the host SVG element.
-}
onHostMouseUp : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseUp fn =
    fn |> MouseHandler |> MouseUp |> registerHostListener


{-| Register an event listener.
-}
registerListener : EventListener msg -> Drawing msg -> Drawing msg
registerListener listener drawing =
    case drawing of
        DrawingEvents events childDrawing ->
            DrawingEvents (eventsAddListener listener events) childDrawing

        _ ->
            DrawingEvents (newEventsWithListener listener) drawing


{-| Register an event listener on the host SVG element.
-}
registerHostListener : EventListener msg -> Drawing msg -> Drawing msg
registerHostListener listener drawing =
    case drawing of
        DrawingHostEvents events childDrawing ->
            DrawingHostEvents (eventsAddListener listener events) childDrawing

        _ ->
            DrawingHostEvents (newEventsWithListener listener) drawing


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
    { offsetPoint : P2
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
                |> (\mat -> Math.p2ApplyAffineTransform mat clientPoint)

        calcPointIn : CSysName -> P2 -> P2
        calcPointIn name clientPoint =
            getCSys name cSysDict
                |> Maybe.withDefault localToWorld
                |> Math.affInvert
                |> (\mat -> Math.p2ApplyAffineTransform mat clientPoint)
    in
    Decode.map3
        (\clientPoint btns mods ->
            { offsetPoint = clientPoint
            , localPoint = calcLocalPoint clientPoint
            , buttons = btns
            , modifiers = mods
            , pointIn = \name -> calcPointIn name clientPoint
            }
        )
        offsetP2
        mouseButtons
        modifiers


offsetP2 : Decoder P2
offsetP2 =
    Decode.map2 Math.p2 offsetXFloat offsetYFloat


offsetXFloat : Decoder Float
offsetXFloat =
    Decode.map toFloat offsetX


offsetYFloat : Decoder Float
offsetYFloat =
    Decode.map toFloat offsetY


mouseButtons : Decoder MouseButtons
mouseButtons =
    let
        buttonState : Int -> Int -> MouseButtonState
        buttonState buttonNumber input =
            if
                Bitwise.and
                    input
                    (Bitwise.shiftLeftBy (buttonNumber - 1) 0x01)
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


offsetX : Decoder Int
offsetX =
    Decode.field "offsetX" Decode.int


offsetY : Decoder Int
offsetY =
    Decode.field "offsetY" Decode.int


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
    let
        ( svgContent, hostAttrs ) =
            renderSvgElement viewBox drawing
    in
    TypedSvg.svg
        ([ SvgAttributes.width (px viewBox.width)
         , SvgAttributes.height (px viewBox.height)
         , SvgAttributes.viewBox
            viewBox.minX
            viewBox.minY
            viewBox.width
            viewBox.height
         , SvgAttributes.preserveAspectRatio (Align ScaleMid ScaleMid) Meet
         ]
            ++ hostAttrs
        )
        [ svgContent
        ]


{-| Render a diagram to an SVG element.
-}
renderSvgElement : ViewBox -> Drawing msg -> ( Svg msg, List (Attribute msg) )
renderSvgElement viewBox drawing =
    renderRecurseDiscardWithDefault (initState viewBox) drawing


{-| Overall state of the evaluator.

This is split into:

1.  Nested state, which strictly follows the nesting of the Drawing.
2.  Threaded state, which is threaded in the direction of a depth-first
    traversal.

-}
type State msg
    = State
        { nested : NestedState msg
        , threaded : ThreadedState msg
        }


stateGetNested : State msg -> NestedState msg
stateGetNested (State state) =
    state.nested


stateGetThreaded : State msg -> ThreadedState msg
stateGetThreaded (State state) =
    state.threaded


{-| State that strictly follows the nesting structure.
-}
type NestedState msg
    = NestedState
        { style : Style msg
        , events : Events msg
        , localToWorld : AffineTransform
        , anchorNS : List AnchorNamespace
        , viewBox : ViewBox
        }


nestedGetStyle : NestedState msg -> Style msg
nestedGetStyle (NestedState nested) =
    nested.style


nestedGetEvents : NestedState msg -> Events msg
nestedGetEvents (NestedState nested) =
    nested.events


nestedGetLocalToWorld : NestedState msg -> AffineTransform
nestedGetLocalToWorld (NestedState nested) =
    nested.localToWorld


nestedGetAnchorNamespaces : NestedState msg -> List AnchorNamespace
nestedGetAnchorNamespaces (NestedState nested) =
    nested.anchorNS


nestedGetViewBox : NestedState msg -> ViewBox
nestedGetViewBox (NestedState nested) =
    nested.viewBox


nestedCombineStyles : Style msg -> NestedState msg -> NestedState msg
nestedCombineStyles childStyle (NestedState parent) =
    NestedState { parent | style = combineStyles parent.style childStyle }


nestedCombineEvents : Events msg -> NestedState msg -> NestedState msg
nestedCombineEvents childEvents (NestedState parent) =
    NestedState { parent | events = combineEvents parent.events childEvents }


nestedSetLocalToWorld : AffineTransform -> NestedState msg -> NestedState msg
nestedSetLocalToWorld l2w (NestedState parent) =
    NestedState { parent | localToWorld = l2w }


nestedComposeTransform : AffineTransform -> NestedState msg -> NestedState msg
nestedComposeTransform childTransform (NestedState parent) =
    nestedSetLocalToWorld
        (Math.affMatMul parent.localToWorld childTransform)
        (NestedState parent)


nestedPrependAnchorNamespace :
    AnchorNamespace
    -> NestedState msg
    -> NestedState msg
nestedPrependAnchorNamespace namespace (NestedState parent) =
    NestedState
        { parent
            | anchorNS = namespace :: parent.anchorNS
        }


{-| State that is threaded through a depth-first evaluation.
-}
type ThreadedState msg
    = ThreadedState
        { coordinateSystems : CSysDict
        , droppedAnchors : AnchorDict
        , hostAttributes : List (Attribute msg)
        }


threadedGetCoordinateSystems : ThreadedState msg -> CSysDict
threadedGetCoordinateSystems (ThreadedState threaded) =
    threaded.coordinateSystems


threadedGetDroppedAnchors : ThreadedState msg -> AnchorDict
threadedGetDroppedAnchors (ThreadedState threaded) =
    threaded.droppedAnchors


threadedGetHostAttributes : ThreadedState msg -> List (Attribute msg)
threadedGetHostAttributes (ThreadedState threaded) =
    threaded.hostAttributes


threadedInsertCoordinateSystem :
    ( CSysName, AffineTransform )
    -> ThreadedState msg
    -> ThreadedState msg
threadedInsertCoordinateSystem pair (ThreadedState parent) =
    ThreadedState
        { parent
            | coordinateSystems = insertCSys pair parent.coordinateSystems
        }


threadedDropAnchor :
    ( AnchorName, P2 )
    -> ThreadedState msg
    -> ThreadedState msg
threadedDropAnchor pair (ThreadedState parent) =
    ThreadedState
        { parent
            | droppedAnchors =
                insertAnchorWorldLocation pair parent.droppedAnchors
        }


threadedAddHostAttributes :
    List (Attribute msg)
    -> ThreadedState msg
    -> ThreadedState msg
threadedAddHostAttributes attrs (ThreadedState parent) =
    ThreadedState
        { parent | hostAttributes = attrs ++ parent.hostAttributes }


initState : ViewBox -> State msg
initState viewBox =
    State
        { nested =
            NestedState
                { style = styleDefault
                , events = emptyEvents
                , localToWorld = initLocalToWorld viewBox
                , anchorNS = []
                , viewBox = viewBox
                }
        , threaded =
            ThreadedState
                { coordinateSystems = emptyCSysDict
                , droppedAnchors = emptyAnchorDict
                , hostAttributes = []
                }
        }


initLocalToWorld : ViewBox -> AffineTransform
initLocalToWorld vb =
    Math.affFromComponents
        [ Math.AffineScaling <| Math.Scaling 1 -1
        , Math.AffineTranslation <| Math.Translation (Math.v2 0 vb.height)
        , Math.AffineTranslation <| Math.Translation (Math.v2 vb.minX vb.minY)
        ]


stateSetThreaded : ThreadedState msg -> State msg -> State msg
stateSetThreaded newThreadedState (State old) =
    State { old | threaded = newThreadedState }


stateSetNested : NestedState msg -> State msg -> State msg
stateSetNested newNestedState (State old) =
    State { old | nested = newNestedState }


stateUpdateNested :
    (NestedState msg -> NestedState msg)
    -> State msg
    -> State msg
stateUpdateNested updater (State old) =
    stateSetNested (updater old.nested) (State old)


stateUpdateThreaded :
    (ThreadedState msg -> ThreadedState msg)
    -> State msg
    -> State msg
stateUpdateThreaded updater (State old) =
    stateSetThreaded (updater old.threaded) (State old)


stateCombineStyles : Style msg -> State msg -> State msg
stateCombineStyles childStyle =
    stateUpdateNested <| nestedCombineStyles childStyle


stateCombineEvents : Events msg -> State msg -> State msg
stateCombineEvents childEvents =
    stateUpdateNested <| nestedCombineEvents childEvents


stateComposeTransform : AffineTransform -> State msg -> State msg
stateComposeTransform childTransform =
    stateUpdateNested <| nestedComposeTransform childTransform


statePrependAnchorNamespace : AnchorNamespace -> State msg -> State msg
statePrependAnchorNamespace ns =
    stateUpdateNested <| nestedPrependAnchorNamespace ns


renderRecurse :
    State msg
    -> Drawing msg
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurse state drawing =
    case drawing of
        DrawingEmpty ->
            renderRecurseEmpty state

        DrawingPath drawPath ->
            renderRecursePath state drawPath

        DrawingSvg svgFunction ->
            renderRecurseSvg state svgFunction

        DrawingStyled sty child ->
            renderRecurseStyled state sty child

        DrawingTransformed affineTransform child ->
            renderRecurseTransformed state affineTransform child

        DrawingGroup children ->
            renderRecurseGroup state children

        DrawingEvents events child ->
            renderRecurseEvents state events child

        DrawingHostEvents events child ->
            renderRecurseHostEvents state events child

        DrawingTagCSys csysName ->
            renderRecurseTagCSys state csysName

        DrawingPrependAnchorNamespace nameCmp child ->
            renderRecursePrependAnchorNamespace state nameCmp child

        DrawingDropAnchor nameCmp location ->
            renderRecurseDropAnchor state nameCmp location

        DrawingWeighAnchors createFn ->
            renderRecurseWeighAnchors state createFn


{-| Same as `renderRecurse`, but it packs the threaded state back into the
original state.
-}
renderRecurseState : State msg -> Drawing msg -> ( State msg, Maybe (Svg msg) )
renderRecurseState state dwg =
    let
        ( threadedState, result ) =
            renderRecurse state dwg
    in
    ( stateSetThreaded threadedState state, result )


{-| Same as `renderRecurse`, but discards the final threaded state.
-}
renderRecurseDiscard :
    State msg
    -> Drawing msg
    -> ( Maybe (Svg msg), List (Attribute msg) )
renderRecurseDiscard state dwg =
    let
        ( threadedState, result ) =
            renderRecurse state dwg
    in
    ( result, threadedGetHostAttributes threadedState )


{-| Same as `renderRecurseDiscard`, but returns a default, empty SVG
if no SVG was produced.
-}
renderRecurseDiscardWithDefault :
    State msg
    -> Drawing msg
    -> ( Svg msg, List (Attribute msg) )
renderRecurseDiscardWithDefault state dwg =
    let
        ( maybeSvg, hostAttrs ) =
            renderRecurseDiscard state dwg
    in
    ( Maybe.withDefault emptySvg maybeSvg, hostAttrs )


{-| A notionally-empty SVG.
-}
emptySvg : Svg msg
emptySvg =
    TypedSvg.g [ SvgAttributes.display DisplayNone ] []


renderRecurseEmpty : State msg -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseEmpty state =
    ( stateGetThreaded state, Nothing )


renderRecursePath :
    State msg
    -> Path
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecursePath state pth =
    let
        l2w =
            state |> stateGetNested >> nestedGetLocalToWorld

        csys =
            state |> stateGetThreaded >> threadedGetCoordinateSystems

        styleAttr =
            state |> stateGetNested >> nestedGetStyle >> styleToAttributes

        eventAttr =
            state
                |> stateGetNested
                >> nestedGetEvents
                >> eventsToAttributes l2w csys

        pathAttr =
            pth
                |> P.pathApplyAffineTransform l2w
                >> SP.formatPath (SP.NFixDigits 2)
                >> SP.svgStringPathToString
                >> SvgAttributes.d

        outMaybeSvg =
            if P.pathIsEmpty pth then
                Nothing

            else
                Just <|
                    TypedSvg.path (pathAttr :: styleAttr ++ eventAttr) []
    in
    ( stateGetThreaded state, outMaybeSvg )


renderRecurseSvg :
    State msg
    -> (AffineTransform -> Svg msg)
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseSvg state svgFunction =
    ( stateGetThreaded state
    , svgFunction
        (stateGetNested >> nestedGetLocalToWorld <| state)
        |> Just
    )


renderRecurseStyled :
    State msg
    -> Style msg
    -> Drawing msg
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseStyled state sty childDrawing =
    renderRecurse (stateCombineStyles sty state) childDrawing


renderRecurseTransformed :
    State msg
    -> AffineTransform
    -> Drawing msg
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseTransformed state xf childDrawing =
    renderRecurse (stateComposeTransform xf state) childDrawing


renderRecurseGroup :
    State msg
    -> List (Drawing msg)
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseGroup state drawings =
    let
        renderRecurseGroupFold :
            Drawing msg
            -> ( State msg, List (Maybe (Svg msg)) )
            -> ( State msg, List (Maybe (Svg msg)) )
        renderRecurseGroupFold dwg ( stepInState, accum ) =
            let
                ( stepOutState, producedSvg ) =
                    renderRecurseState stepInState dwg
            in
            ( stepOutState, producedSvg :: accum )

        groupSvg : List (Maybe (Svg msg)) -> Maybe (Svg msg)
        groupSvg maybeSvgs =
            List.filterMap identity maybeSvgs
                |> (\svgList ->
                        if List.isEmpty svgList then
                            Nothing

                        else
                            List.reverse svgList |> TypedSvg.g [] |> Just
                   )
    in
    List.foldl renderRecurseGroupFold ( state, [] ) drawings
        |> (\( finalState, maybeSvgs ) ->
                ( stateGetThreaded finalState
                , groupSvg maybeSvgs
                )
           )


renderRecurseEvents :
    State msg
    -> Events msg
    -> Drawing msg
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseEvents state events childDrawing =
    renderRecurse (stateCombineEvents events state) childDrawing


renderRecurseHostEvents :
    State msg
    -> Events msg
    -> Drawing msg
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseHostEvents state events childDrawing =
    let
        hostAttrs =
            eventsToAttributes
                (state |> stateGetNested >> nestedGetLocalToWorld)
                (state |> stateGetThreaded >> threadedGetCoordinateSystems)
                events

        newState =
            state |> stateUpdateThreaded (threadedAddHostAttributes hostAttrs)
    in
    renderRecurse newState childDrawing


renderRecurseTagCSys :
    State msg
    -> CSysName
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseTagCSys state cSysName =
    ( stateGetThreaded state
        |> threadedInsertCoordinateSystem
            ( cSysName
            , (stateGetNested >> nestedGetLocalToWorld) state
            )
    , Nothing
    )


renderRecursePrependAnchorNamespace :
    State msg
    -> AnchorNamespace
    -> Drawing msg
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecursePrependAnchorNamespace state ns childDrawing =
    renderRecurse (statePrependAnchorNamespace ns state) childDrawing


renderRecurseDropAnchor :
    State msg
    -> AnchorName
    -> P2
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseDropAnchor state name localPt =
    ( state
        |> stateGetThreaded
        |> threadedDropAnchor
            ( anchorNamePrependAll
                (state |> stateGetNested >> nestedGetAnchorNamespaces)
                name
            , Math.p2ApplyAffineTransform
                (state |> stateGetNested >> nestedGetLocalToWorld)
                localPt
            )
    , Nothing
    )


renderRecurseWeighAnchors :
    State msg
    -> ((String -> ( Float, Float )) -> Drawing msg)
    -> ( ThreadedState msg, Maybe (Svg msg) )
renderRecurseWeighAnchors state createFn =
    let
        anchorFn : String -> ( Float, Float )
        anchorFn aname =
            state
                |> stateGetThreaded
                >> threadedGetDroppedAnchors
                >> getAnchorWorldLocation (anchorName aname)
                >> Maybe.map
                    (Math.p2ApplyAffineTransform
                        (state
                            |> stateGetNested
                            >> nestedGetLocalToWorld
                            >> Math.affInvert
                        )
                    )
                >> Maybe.map (\p -> ( Math.p2x p, Math.p2y p ))
                >> Maybe.withDefault ( 0, 0 )

        createdDrawing : Drawing msg
        createdDrawing =
            createFn anchorFn
    in
    renderRecurse state createdDrawing


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
                , toAttr styleGetStrokeLinecap SvgAttributes.strokeLinecap
                , toAttr styleGetStrokeLinejoin SvgAttributes.strokeLinejoin
                ]
    in
    styleAttrs ++ List.reverse (styleGetAttributes styl)


{-| Map a function over the message type of a drawing.
-}
map : (a -> msg) -> Drawing a -> Drawing msg
map f drawing =
    case drawing of
        DrawingEmpty ->
            DrawingEmpty

        DrawingPath pth ->
            DrawingPath pth

        DrawingSvg mkSvg ->
            DrawingSvg <|
                \xform -> mkSvg xform |> TypedSvgCore.map f

        DrawingStyled sty child ->
            DrawingStyled (mapStyle f sty) (map f child)

        DrawingTransformed xform child ->
            DrawingTransformed xform (map f child)

        DrawingGroup children ->
            DrawingGroup <| List.map (map f) children

        DrawingEvents events child ->
            DrawingEvents (mapEvents f events) (map f child)

        DrawingHostEvents events child ->
            DrawingHostEvents (mapEvents f events) (map f child)

        DrawingTagCSys name ->
            DrawingTagCSys name

        DrawingPrependAnchorNamespace ns child ->
            DrawingPrependAnchorNamespace ns (map f child)

        DrawingDropAnchor name location ->
            DrawingDropAnchor name location

        DrawingWeighAnchors fn ->
            DrawingWeighAnchors <| \extractFn -> fn extractFn |> map f


mapStyle : (a -> msg) -> Style a -> Style msg
mapStyle f (Style sty) =
    Style
        { fill = sty.fill
        , stroke = sty.stroke
        , strokeWidth = sty.strokeWidth
        , strokeLinecap = sty.strokeLinecap
        , strokeLinejoin = sty.strokeLinejoin
        , extraAttributes = List.map (mapAttribute f) sty.extraAttributes
        }


mapEvents : (a -> msg) -> Events a -> Events msg
mapEvents f (Events listeners) =
    Events <| List.map (mapEventListener f) listeners


mapEventListener : (a -> msg) -> EventListener a -> EventListener msg
mapEventListener f listener =
    case listener of
        MouseClick h ->
            MouseClick <| mapMouseHandler f h

        MouseContextMenu h ->
            MouseContextMenu <| mapMouseHandler f h

        MouseDblClick h ->
            MouseDblClick <| mapMouseHandler f h

        MouseDown h ->
            MouseDown <| mapMouseHandler f h

        MouseEnter h ->
            MouseEnter <| mapMouseHandler f h

        MouseLeave h ->
            MouseLeave <| mapMouseHandler f h

        MouseMove h ->
            MouseMove <| mapMouseHandler f h

        MouseOut h ->
            MouseOut <| mapMouseHandler f h

        MouseOver h ->
            MouseOver <| mapMouseHandler f h

        MouseUp h ->
            MouseUp <| mapMouseHandler f h


mapMouseHandler : (a -> msg) -> MouseHandler a -> MouseHandler msg
mapMouseHandler f (MouseHandler fn) =
    MouseHandler (fn >> f)



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



---- Anchors ------------------------------------------------------------------


{-| Anchor namespace.
-}
type AnchorNamespace
    = AnchorNamespace String


{-| Create an `AnchorNamespace`.

The string name of an `AnchorNamespace` cannot contain `'.'` characters. If a
string is supplied containing these characters, they will be filtered out.

-}
anchorNamespace : String -> AnchorNamespace
anchorNamespace ns =
    String.filter (\c -> c /= '.') ns |> AnchorNamespace


{-| Anchor name.
-}
type AnchorName
    = AnchorName (List AnchorNamespace) String


{-| Create an `AnchorName`.

If any `'.'` characters are supplied in the name then they are assumed to
separate namespaces.

-}
anchorName : String -> AnchorName
anchorName name =
    String.split "." name
        |> (\strs ->
                case List.reverse strs of
                    [] ->
                        AnchorName [] ""

                    n :: [] ->
                        AnchorName [] n

                    n :: ns ->
                        AnchorName
                            (List.reverse ns |> List.map anchorNamespace)
                            n
           )


{-| Prepend a new namespace to an existing anchor name.
-}
anchorNamePrependAll : List AnchorNamespace -> AnchorName -> AnchorName
anchorNamePrependAll newns (AnchorName ns n) =
    AnchorName (newns ++ ns) n


{-| Convert an anchor name to a string.
-}
anchorNameToString : AnchorName -> String
anchorNameToString (AnchorName ns n) =
    (List.map (\(AnchorNamespace namespaceStr) -> namespaceStr) ns
        |> String.join "."
    )
        ++ "."
        ++ n


{-| Dictionary of anchor names (as strings) to their world coordinates.
-}
type AnchorDict
    = AnchorDict (Dict String P2)


{-| Create an empty `AnchorDict`.
-}
emptyAnchorDict : AnchorDict
emptyAnchorDict =
    AnchorDict <| Dict.empty


{-| Insert an anchor location into the dictionary.
-}
insertAnchorWorldLocation : ( AnchorName, P2 ) -> AnchorDict -> AnchorDict
insertAnchorWorldLocation ( aname, location ) (AnchorDict dict) =
    AnchorDict <| Dict.insert (anchorNameToString aname) location dict


{-| Get the location of an anchor from the dictionary in world coordinates.
-}
getAnchorWorldLocation : AnchorName -> AnchorDict -> Maybe P2
getAnchorWorldLocation aname (AnchorDict dict) =
    Dict.get (anchorNameToString aname) dict
