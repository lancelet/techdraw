module Techdraw.Internal.Svg.Defs exposing
    ( Defs
    , empty
    , ensureLinearGradient, ensureRadialGradient
    , toSvg
    )

{-| Manage items that can appear in an SVG `<defs>` element for the produced
document.

@docs Defs
@docs empty
@docs ensureLinearGradient, ensureRadialGradient
@docs toSvg

-}

import Color
import Dict exposing (Dict)
import Techdraw.Internal.Hash as Hash exposing (Hasher)
import Techdraw.Internal.Svg.Extras as Extras
import Techdraw.Internal.Svg.Path as SvgPath
import Techdraw.Math as Math
import Techdraw.Style as Style
    exposing
        ( LinearGradient
        , RadialGradient
        , radialGradient
        )
import TypedSvg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Core exposing (Attribute, Svg, attribute)
import TypedSvg.Types exposing (CoordinateSystem(..), px)


{-| Item that can appear in a `<defs>`.
-}
type DefItem
    = DefLinearGradient LinearGradient
    | DefRadialGradient RadialGradient


{-| Defs element.
-}
type Defs
    = Defs (Dict String DefItem)


{-| Empty defs.
-}
empty : Defs
empty =
    Defs Dict.empty


{-| Ensure a hashable item appears in the `Defs`.
-}
ensure : Hasher a -> (a -> DefItem) -> a -> Defs -> Defs
ensure hashFn itemFn item (Defs defs) =
    Defs <| Dict.insert (hashFn item |> Hash.toHex) (itemFn item) defs


{-| Ensure a `LinearGradient` appears in the `Defs`.
-}
ensureLinearGradient : LinearGradient -> Defs -> Defs
ensureLinearGradient =
    ensure Style.hashLinearGradient DefLinearGradient


{-| Ensure a `RadialGradient` appears in the `Defs`.
-}
ensureRadialGradient : RadialGradient -> Defs -> Defs
ensureRadialGradient =
    ensure Style.hashRadialGradient DefRadialGradient



---- Conversion to SVG --------------------------------------------------------


{-| Convert a `Defs` value to an `<defs>` element.
-}
toSvg : Defs -> Svg msg
toSvg (Defs defs) =
    TypedSvg.defs [] (defs |> Dict.values |> List.map defItemToSvg)


{-| Convert a DefItem to its corresponding SVG.
-}
defItemToSvg : DefItem -> Svg msg
defItemToSvg item =
    case item of
        DefLinearGradient lg ->
            linearGradientToSvg lg

        DefRadialGradient rg ->
            radialGradientToSvg rg


{-| Convert a `LinearGradient` to SVG.
-}
linearGradientToSvg : Style.LinearGradient -> Svg msg
linearGradientToSvg linearGradient =
    let
        hash =
            Style.hashLinearGradient linearGradient

        params =
            Style.linearGradientParams linearGradient
    in
    TypedSvg.linearGradient
        [ hash |> Hash.toHex |> SvgAttr.id
        , SvgAttr.gradientUnits CoordinateSystemUserSpaceOnUse
        , params.start |> Math.p2x |> px |> SvgAttr.x1
        , params.start |> Math.p2y |> px |> SvgAttr.y1
        , params.end |> Math.p2x |> px |> SvgAttr.x2
        , params.end |> Math.p2y |> px |> SvgAttr.y2
        , params.transform
            |> Extras.convertAffineTransformToSvg
            |> (\matrix -> SvgAttr.gradientTransform [ matrix ])
        ]
        (params |> .gradient |> gradientToSvg)


{-| Convert a `RadialGradient` to SVG.
-}
radialGradientToSvg : Style.RadialGradient -> Svg msg
radialGradientToSvg radialGradient =
    let
        hash =
            Style.hashRadialGradient radialGradient

        params =
            Style.radialGradientParams radialGradient
    in
    TypedSvg.radialGradient
        [ hash |> Hash.toHex |> SvgAttr.id
        , SvgAttr.gradientUnits CoordinateSystemUserSpaceOnUse
        , params.innerCenter |> Math.p2x |> px |> SvgAttr.fx
        , params.innerCenter |> Math.p2y |> px |> SvgAttr.fy
        , params.innerRadius |> attrFr
        , params.outerCenter |> Math.p2x |> px |> SvgAttr.cx
        , params.outerCenter |> Math.p2y |> px |> SvgAttr.cy
        , params.outerRadius |> px |> SvgAttr.r

        -- TODO: AffineTransform
        ]
        (params |> .gradient |> gradientToSvg)


{-| Convert a `Gradient` to a list of SVG stop elements.
-}
gradientToSvg : Style.Gradient -> List (Svg msg)
gradientToSvg =
    Style.gradientParams >> List.map stopToSvg


{-| Convert a gradient `Stop` to SVG.
-}
stopToSvg : Style.Stop -> Svg msg
stopToSvg stop =
    TypedSvg.stop
        [ stop |> Style.stopLocation |> strPercent |> SvgAttr.offset
        , stop |> Style.stopColor |> Color.toCssString |> SvgAttr.stopColor
        ]
        []


{-| The "fr" attribute.

<https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fr>

-}
attrFr : Float -> Attribute msg
attrFr value =
    attribute
        "fr"
        (SvgPath.toFixed (SvgPath.NFixDigits 2) value ++ "px")


{-| Create a string with a percent sign and one significant figure after
the decimal point.

The value passed in should typically be in the range [0, 1] to map to a
percentage value of [0%, 100%].

-}
strPercent : Float -> String
strPercent x =
    SvgPath.toFixed (SvgPath.NFixDigits 1) (x * 100) ++ "%"
