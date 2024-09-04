module Techdraw exposing
    ( Drawing
    , empty, path, atop, below
    , fill, fillRule, stroke, strokeWidth, lineCap, lineJoin
    , dashArray, dashOffset
    , transform, translate, rotateDegrees, rotateDegreesAbout
    , skewXDegrees, scale
    , onEvent
    , toSvg, toSvgWithWarnings
    )

{-| Techdraw: Interactive Technical Drawing in Elm


# Creating Drawings

@docs Drawing
@docs empty, path, atop, below


# Styling Drawings

@docs fill, fillRule, stroke, strokeWidth, lineCap, lineJoin
@docs dashArray, dashOffset


# Transforming Drawings

@docs transform, translate, rotateDegrees, rotateDegreesAbout
@docs skewXDegrees, scale


# Adding Handlers for Events

@docs onEvent


# Converting Drawings to SVG

@docs toSvg, toSvgWithWarnings

-}

import Html exposing (Html)
import Techdraw.Event exposing (EventHandler)
import Techdraw.Internal.Dwg exposing (Dwg(..))
import Techdraw.Internal.StyleAtom as StyleAtom exposing (StyleAtom)
import Techdraw.Internal.Svg.Machine as DwgMachine
import Techdraw.Math as Math exposing (AffineTransform, P2, V2)
import Techdraw.Path exposing (Path)
import Techdraw.Style as Style
import Techdraw.Types exposing (Sizing)



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
below : Drawing msg -> Drawing msg -> Drawing msg
below bottom top =
    Drawing (DwgBelow (unDrawing bottom) (unDrawing top))



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



---- Adding Handlers for Events -----------------------------------------------


{-| Add an event handler to an existing drawing.
-}
onEvent : EventHandler msg -> Drawing msg -> Drawing msg
onEvent eventHandler =
    unDrawing >> DwgEventHandler eventHandler >> Drawing



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
