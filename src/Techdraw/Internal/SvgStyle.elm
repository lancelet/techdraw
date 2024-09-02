module Techdraw.Internal.SvgStyle exposing
    ( Defs, SvgStyle(..)
    , emptyDefs, defsUnion
    , styleToSvgAttributes
    )

{-| Convert styling information to SVG.

@docs Defs, SvgStyle
@docs emptyDefs, defsUnion
@docs styleToSvgAttributes

-}

import Dict exposing (Dict)
import Techdraw.Math as Math
import Techdraw.Style as Style
    exposing
        ( Gradient
        , LinearGradient(..)
        , RadialGradient
        , Style
        )
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Attribute, Svg)
import TypedSvg.Types exposing (px)



---- Defs collection ----------------------------------------------------------


{-| Item that can be included in `defs`.
-}
type DefItem
    = DefGradient Gradient
    | DefLinearGradient LinearGradient
    | DefRadialGradient RadialGradient


{-| A dictionary of hashes to defs items.
-}
type Defs
    = Defs (Dict String DefItem)


{-| Empty defs items.
-}
emptyDefs : Defs
emptyDefs =
    Defs Dict.empty


{-| Combine defs.
-}
defsUnion : Defs -> Defs -> Defs
defsUnion (Defs d1) (Defs d2) =
    Defs (Dict.union d1 d2)


{-| Include a gradient in the defs items.
-}
defsIncludeGradient : Gradient -> Defs -> Defs
defsIncludeGradient gradient (Defs dict) =
    let
        key =
            Style.gradientHexHash gradient
    in
    if Dict.member key dict then
        Defs dict

    else
        Defs <| Dict.insert key (DefGradient gradient) dict


{-| Include a LinearGradient and its child Gradient in defs items.
-}
defsIncludeLinearGradient : LinearGradient -> Defs -> Defs
defsIncludeLinearGradient linearGradient (Defs dict) =
    let
        key =
            Style.linearGradientHexHash linearGradient
    in
    if Dict.member key dict then
        Defs dict

    else
        (Defs <| Dict.insert key (DefLinearGradient linearGradient) dict)
            |> defsIncludeGradient
                (Style.linearGradientParams linearGradient |> .gradient)


{-| Include a RadialGradient and its child Gradient in defs items.
-}
defsIncludeRadialGradient : RadialGradient -> Defs -> Defs
defsIncludeRadialGradient radialGradient (Defs dict) =
    let
        key =
            Style.radialGradientHexHash radialGradient
    in
    if Dict.member key dict then
        Defs dict

    else
        (Defs <| Dict.insert key (DefRadialGradient radialGradient) dict)
            |> defsIncludeGradient
                (Style.radialGradientParams radialGradient |> .gradient)



----


{-| Return value from SVG styling.
-}
type SvgStyle msg
    = SvgStyle
        { attributes : List (Attribute msg)
        , defs : Defs
        }


{-| Convert a `Style` to a list of SVG `Attribute`s.
-}
styleToSvgAttributes : Style -> SvgStyle msg
styleToSvgAttributes =
    Debug.todo "TODO"
