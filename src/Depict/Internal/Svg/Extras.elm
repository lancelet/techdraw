module Depict.Internal.Svg.Extras exposing (convertAffineTransformToSvg)

{-| Extra SVG conversions that don't belong somewhere more specific.

@docs convertAffineTransformToSvg

-}

import Depict.Math
    exposing
        ( AffineTransform
        , affGetLinear
        , affGetTranslation
        , m22e11
        , m22e12
        , m22e21
        , m22e22
        , v2e1
        , v2e2
        )
import TypedSvg.Types as SvgTypes


{-| Convert an `AffineTransform` to a `TypedSvg` `Transform.Matrix`.
-}
convertAffineTransformToSvg : AffineTransform -> SvgTypes.Transform
convertAffineTransformToSvg affineTransform =
    let
        m =
            affGetLinear affineTransform

        t =
            affGetTranslation affineTransform
    in
    SvgTypes.Matrix
        (m22e11 m)
        (m22e21 m)
        (m22e12 m)
        (m22e22 m)
        (v2e1 t)
        (v2e2 t)
