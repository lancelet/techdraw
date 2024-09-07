module Techdraw exposing
    ( Drawing
    , empty, path, atop, beneath, stack
    , fill, fillRule, stroke, strokeWidth, lineCap, lineJoin
    , dashArray, dashOffset
    , transform, translate, rotateDegrees, rotateDegreesAbout
    , skewXDegrees, scale
    , freeze, use
    , onEvent
    , onMouseClick, onMouseContextMenu, onMouseDblClick, onMouseDown
    , onMouseEnter, onMouseLeave, onMouseMove, onMouseOut, onMouseOver
    , onMouseUp
    , onHostMouseClick, onHostMouseContextMenu, onHostMouseDblClick
    , onHostMouseDown, onHostMouseEnter, onHostMouseLeave, onHostMouseMove
    , onHostMouseOut, onHostMouseOver, onHostMouseUp
    , tagCSys
    , toSvg, toSvgWithWarnings
    )

{-| Techdraw: Interactive Technical Drawing in Elm


# Creating Drawings

@docs Drawing
@docs empty, path, atop, beneath, stack


# Styling Drawings

@docs fill, fillRule, stroke, strokeWidth, lineCap, lineJoin
@docs dashArray, dashOffset


# Transforming Drawings

@docs transform, translate, rotateDegrees, rotateDegreesAbout
@docs skewXDegrees, scale


# Freezing Drawings

@docs freeze, use


# Adding Handlers for Events

@docs onEvent
@docs onMouseClick, onMouseContextMenu, onMouseDblClick, onMouseDown
@docs onMouseEnter, onMouseLeave, onMouseMove, onMouseOut, onMouseOver
@docs onMouseUp
@docs onHostMouseClick, onHostMouseContextMenu, onHostMouseDblClick
@docs onHostMouseDown, onHostMouseEnter, onHostMouseLeave, onHostMouseMove
@docs onHostMouseOut, onHostMouseOver, onHostMouseUp


# Tagging Coordinate Systems

@docs tagCSys


# Converting Drawings to SVG

@docs toSvg, toSvgWithWarnings

-}

import Html exposing (Html)
import Techdraw.Event as Event exposing (EventHandler, MouseInfo)
import Techdraw.Internal.Dwg exposing (Dwg(..))
import Techdraw.Internal.StyleAtom as StyleAtom exposing (StyleAtom)
import Techdraw.Internal.Svg.Machine as DwgMachine
import Techdraw.Math as Math exposing (AffineTransform, P2, V2)
import Techdraw.Path exposing (Path)
import Techdraw.Style as Style
import Techdraw.Types exposing (CSysName, FrozenName, Order(..), Sizing)



---- Creating Drawings --------------------------------------------------------


{-| A drawing.
-}
type Drawing msg
    = Drawing (Dwg msg)


{-| Un-apply the `Drawing` newtype.
-}
unDrawing : Drawing msg -> Dwg msg
unDrawing (Drawing dwg) =
    dwg


{-| Create an empty `Drawing`.
-}
empty : Drawing msg
empty =
    Drawing DwgEmpty


{-| Create a `Drawing` of a `Path`.
-}
path : Path -> Drawing msg
path =
    DwgPath >> Drawing


{-| Draw the first drawing on top of the second drawing.

The first/top drawing is processed first.

-}
atop : Drawing msg -> Drawing msg -> Drawing msg
atop top bottom =
    Drawing (DwgAtop (unDrawing top) (unDrawing bottom))


{-| Draw the first drawing below the second drawing.

The first/bottom drawing is processed first.

-}
beneath : Drawing msg -> Drawing msg -> Drawing msg
beneath bottom top =
    Drawing (DwgBeneath (unDrawing bottom) (unDrawing top))


{-| Stack drawings in either a bottom-to-top or top-to-bottom order.
-}
stack : Order -> List (Drawing msg) -> Drawing msg
stack order =
    case order of
        TopToBottom ->
            List.foldl beneath empty

        BottomToTop ->
            List.foldl atop empty



---- Styling Drawings ---------------------------------------------------------


{-| Helper function to apply a `StyleAtom` to a `Drawing`.
-}
applyStyleAtom : (a -> StyleAtom) -> a -> Drawing msg -> Drawing msg
applyStyleAtom createFn value childDrawing =
    Drawing (DwgStyled (createFn value) (unDrawing childDrawing))


{-| Set the fill paint.
-}
fill : Style.Paint -> Drawing msg -> Drawing msg
fill =
    applyStyleAtom StyleAtom.Fill


{-| Set the fill rule.
-}
fillRule : Style.FillRule -> Drawing msg -> Drawing msg
fillRule =
    applyStyleAtom StyleAtom.FillRule


{-| Set the stroke paint.
-}
stroke : Style.Paint -> Drawing msg -> Drawing msg
stroke =
    applyStyleAtom StyleAtom.Stroke


{-| Set the stroke width.
-}
strokeWidth : Float -> Drawing msg -> Drawing msg
strokeWidth =
    applyStyleAtom StyleAtom.StrokeWidth


{-| Set the line cap.
-}
lineCap : Style.LineCap -> Drawing msg -> Drawing msg
lineCap =
    applyStyleAtom StyleAtom.LineCap


{-| Set the line join.
-}
lineJoin : Style.LineJoin -> Drawing msg -> Drawing msg
lineJoin =
    applyStyleAtom StyleAtom.LineJoin


{-| Set the dash array.
-}
dashArray : Style.DashArray -> Drawing msg -> Drawing msg
dashArray =
    applyStyleAtom StyleAtom.DashArray


{-| Set the dash offset.
-}
dashOffset : Float -> Drawing msg -> Drawing msg
dashOffset =
    applyStyleAtom StyleAtom.DashOffset



---- Transforming Drawings ----------------------------------------------------


{-| Transform a drawing using an affine transformation.
-}
transform : AffineTransform -> Drawing msg -> Drawing msg
transform affineTransform =
    unDrawing >> DwgTransformed affineTransform >> Drawing


{-| Translate a drewing by the supplied offset vector.
-}
translate : V2 -> Drawing msg -> Drawing msg
translate =
    Math.Translation >> Math.affTranslation >> transform


{-| Rotate a drawing by the specified number of degrees about the origin.
-}
rotateDegrees : Float -> Drawing msg -> Drawing msg
rotateDegrees =
    Math.toRadians
        >> Math.angle2Pi
        >> Math.Rotation
        >> Math.affRotation
        >> transform


{-| Rotate a drawing by the specified number of degrees about a given point.
-}
rotateDegreesAbout : Float -> P2 -> Drawing msg -> Drawing msg
rotateDegreesAbout angle pt =
    transform <|
        Math.affFromComponents
            [ Math.AffineTranslation <|
                Math.Translation <|
                    Math.v2Neg <|
                        Math.p2v pt
            , Math.AffineRotation <|
                Math.Rotation <|
                    Math.angle2Pi <|
                        Math.toRadians angle
            , Math.AffineTranslation <|
                Math.Translation <|
                    Math.p2v pt
            ]


{-| Skew along the x-axis by a value in degrees.
-}
skewXDegrees : Float -> Drawing msg -> Drawing msg
skewXDegrees =
    Math.toRadians >> tan >> Math.ShearingX >> Math.affShearingX >> transform


{-| Scale by the provided factors along the x and y axes.
-}
scale : Float -> Float -> Drawing msg -> Drawing msg
scale scaleX scaleY =
    Math.Scaling scaleX scaleY |> Math.affScaling |> transform



---- Freezing Drawings --------------------------------------------------------


{-| Freeze a drawing, adding an optional name to it.
-}
freeze : Maybe FrozenName -> Drawing msg -> Drawing msg
freeze optName =
    unDrawing >> DwgFrozen optName >> Drawing


{-| Re-use a frozen drawing, summoning it by name.
-}
use : FrozenName -> Drawing msg
use =
    DwgUse >> Drawing



---- Adding Handlers for Events -----------------------------------------------


{-| Add an event handler to an existing drawing.
-}
onEvent : EventHandler msg -> Drawing msg -> Drawing msg
onEvent eventHandler =
    unDrawing >> DwgEventHandler eventHandler >> Drawing


{-| Helper function ao add a mouse event.
-}
onMouseEvent :
    (Event.MouseHandler msg -> EventHandler msg)
    -> (MouseInfo -> msg)
    -> Drawing msg
    -> Drawing msg
onMouseEvent createEventHandler createMsg =
    onEvent (Event.MouseHandler createMsg |> createEventHandler)


{-| Add a mouse click handler to an existing drawing.
-}
onMouseClick : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseClick =
    onMouseEvent Event.MouseClick


{-| Add a mouse context menu handler to an existing drawing.
-}
onMouseContextMenu : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseContextMenu =
    onMouseEvent Event.MouseContextMenu


{-| Add a mouse double click handler to an existing drawing.
-}
onMouseDblClick : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseDblClick =
    onMouseEvent Event.MouseDblClick


{-| Add a mouse button down handler to an existing drawing.
-}
onMouseDown : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseDown =
    onMouseEvent Event.MouseDown


{-| Add a mouse enter handler to an existing drawing.
-}
onMouseEnter : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseEnter =
    onMouseEvent Event.MouseEnter


{-| Add a mouse leave handler to an existing drawing.
-}
onMouseLeave : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseLeave =
    onMouseEvent Event.MouseLeave


{-| Add a mouse move event handler to an existing drawing.
-}
onMouseMove : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseMove =
    onMouseEvent Event.MouseMove


{-| Add a mouse out event handler to an existing drawing.
-}
onMouseOut : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseOut =
    onMouseEvent Event.MouseOut


{-| Add a mouse over event handler to an existing drawing.
-}
onMouseOver : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseOver =
    onMouseEvent Event.MouseOver


{-| Add a mouse up event handler to an existing drawing.
-}
onMouseUp : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onMouseUp =
    onMouseEvent Event.MouseUp


{-| Add a host event handler to an existing drawing.
-}
onHostEvent : EventHandler msg -> Drawing msg -> Drawing msg
onHostEvent eventHandler =
    unDrawing >> DwgHostEventHandler eventHandler >> Drawing


{-| Helper function ao add a mouse event to the host.
-}
onHostMouseEvent :
    (Event.MouseHandler msg -> EventHandler msg)
    -> (MouseInfo -> msg)
    -> Drawing msg
    -> Drawing msg
onHostMouseEvent createEventHandler createMsg =
    onHostEvent (Event.MouseHandler createMsg |> createEventHandler)


{-| Add a mouse click handler to the host of an existing drawing.
-}
onHostMouseClick : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseClick =
    onHostMouseEvent Event.MouseClick


{-| Add a mouse context menu handler to the host of an existing drawing.
-}
onHostMouseContextMenu : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseContextMenu =
    onHostMouseEvent Event.MouseContextMenu


{-| Add a mouse double click handler to the host of an existing drawing.
-}
onHostMouseDblClick : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseDblClick =
    onHostMouseEvent Event.MouseDblClick


{-| Add a mouse button down handler to the host of an existing drawing.
-}
onHostMouseDown : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseDown =
    onHostMouseEvent Event.MouseDown


{-| Add a mouse enter handler to the host of an existing drawing.
-}
onHostMouseEnter : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseEnter =
    onHostMouseEvent Event.MouseEnter


{-| Add a mouse leave handler to the host of an existing drawing.
-}
onHostMouseLeave : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseLeave =
    onHostMouseEvent Event.MouseLeave


{-| Add a mouse move event handler to the host of an existing drawing.
-}
onHostMouseMove : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseMove =
    onHostMouseEvent Event.MouseMove


{-| Add a mouse out event handler to the host of an existing drawing.
-}
onHostMouseOut : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseOut =
    onHostMouseEvent Event.MouseOut


{-| Add a mouse over event handler to the host of an existing drawing.
-}
onHostMouseOver : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseOver =
    onHostMouseEvent Event.MouseOver


{-| Add a mouse up event handler to the host of an existing drawing.
-}
onHostMouseUp : (MouseInfo -> msg) -> Drawing msg -> Drawing msg
onHostMouseUp =
    onHostMouseEvent Event.MouseUp



---- Tagging Coordinate Systems -----------------------------------------------


{-| Tag the current local-to-world transformation with the supplied
coordinate system name.

This coordinate system name can be used with later mouse events to retrieve
coordinates in the named coordinate system.

-}
tagCSys : CSysName -> Drawing msg -> Drawing msg
tagCSys cSysName =
    unDrawing >> DwgTagCSys cSysName >> Drawing



---- Converting Drawings To SVG -----------------------------------------------


{-| Convert a `Drawing` to `Html`.
-}
toSvg : Sizing -> Drawing msg -> Html msg
toSvg sizing =
    unDrawing >> DwgMachine.toSvg sizing >> DwgMachine.svgResultGetHtml


{-| Convert a `Drawing` to `Html`, additionally returning a list of
strings containing any warnings generated while producing the drawing.
-}
toSvgWithWarnings : Sizing -> Drawing msg -> ( Html msg, List String )
toSvgWithWarnings sizing =
    unDrawing >> DwgMachine.toSvg sizing >> DwgMachine.svgResultTuple
