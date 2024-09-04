module Techdraw exposing
    ( Drawing
    , empty, path
    , fill, fillRule, stroke, strokeWidth, lineCap, lineJoin
    , dashArray, dashOffset
    , toSvg, toSvgWithWarnings
    )

{-| Techdraw: Interactive Technical Drawing in Elm


# Creating Drawings

@docs Drawing
@docs empty, path


# Styling Drawings

@docs fill, fillRule, stroke, strokeWidth, lineCap, lineJoin
@docs dashArray, dashOffset


# Converting Drawings to SVG

@docs toSvg, toSvgWithWarnings

-}

import Html exposing (Html)
import Techdraw.Internal.Dwg exposing (Dwg(..))
import Techdraw.Internal.StyleAtom as StyleAtom exposing (StyleAtom)
import Techdraw.Internal.Svg.Machine as DwgMachine
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
