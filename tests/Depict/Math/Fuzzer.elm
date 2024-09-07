module Depict.Math.Fuzzer exposing
    ( floatWideAngle
    , angle2Pi, anglePi, orientationPi
    , rotation, scaling, shearingX, translation
    , affineComponent, affineComponents
    , v2, n2
    , m22
    , p2
    , affineTransform
    )

{-| Fuzzers for the math types.

@docs floatWideAngle
@docs angle2Pi, anglePi, orientationPi
@docs rotation, scaling, shearingX, translation
@docs affineComponent, affineComponents
@docs v2, n2
@docs m22
@docs p2
@docs affineTransform

-}

import Depict.Math as M
import Fuzz exposing (Fuzzer)


{-| Fuzzer for a `Float` that covers a wide angle range.
-}
floatWideAngle : Fuzzer Float
floatWideAngle =
    Fuzz.floatRange (-8 * pi) (8 * pi)


{-| Fuzzer for `Angle2Pi`.
-}
angle2Pi : Fuzzer M.Angle2Pi
angle2Pi =
    Fuzz.map M.angle2Pi floatWideAngle


{-| Fuzzer for `AnglePi`.
-}
anglePi : Fuzzer M.AnglePi
anglePi =
    Fuzz.map M.anglePi floatWideAngle


{-| Fuzzer for `OrientationPi`.
-}
orientationPi : Fuzzer M.OrientationPi
orientationPi =
    Fuzz.map M.orientationPi floatWideAngle


{-| Fuzzer for `Rotation`.
-}
rotation : Fuzzer M.Rotation
rotation =
    Fuzz.map M.Rotation angle2Pi


singleScalePlus : Fuzzer Float
singleScalePlus =
    Fuzz.floatRange 0.2 3.0


singleScaleMinus : Fuzzer Float
singleScaleMinus =
    Fuzz.map negate singleScalePlus


singleScale : Fuzzer Float
singleScale =
    Fuzz.oneOf [ singleScalePlus, singleScaleMinus ]


{-| Fuzzer for `Scaling`.
-}
scaling : Fuzzer M.Scaling
scaling =
    Fuzz.map2
        (\scaleX scaleY -> M.Scaling scaleX scaleY)
        singleScale
        singleScale


shearPlus : Fuzzer Float
shearPlus =
    Fuzz.floatRange 0.2 3.0


shearMinus : Fuzzer Float
shearMinus =
    Fuzz.map negate shearPlus


{-| Fuzzer for `ShearingX`.
-}
shearingX : Fuzzer M.ShearingX
shearingX =
    Fuzz.map M.ShearingX <| Fuzz.oneOf [ shearPlus, shearMinus ]


{-| Fuzzer for `Translation`.
-}
translation : Fuzzer M.Translation
translation =
    Fuzz.map M.Translation v2


{-| Fuzzer for `AffineComponent`.
-}
affineComponent : Fuzzer M.AffineComponent
affineComponent =
    Fuzz.oneOf
        [ Fuzz.map M.AffineRotation rotation
        , Fuzz.map M.AffineScaling scaling
        , Fuzz.map M.AffineShearingX shearingX
        , Fuzz.map M.AffineTranslation translation
        ]


{-| Fuzzer for a list of `AffineComponent`.
-}
affineComponents : Fuzzer (List M.AffineComponent)
affineComponents =
    Fuzz.listOfLengthBetween 1 10 affineComponent


singleCoordinate : Fuzzer Float
singleCoordinate =
    Fuzz.floatRange -100 100


{-| Fuzzer for `V2`.
-}
v2 : Fuzzer M.V2
v2 =
    Fuzz.map2 (\e1 e2 -> M.v2 e1 e2) singleCoordinate singleCoordinate


{-| Fuzzer for `N2`.
-}
n2 : Fuzzer M.N2
n2 =
    Fuzz.map M.v2n v2


{-| Fuzzer for `M22`.

`M22` values are fuzzed by constructing an affine transform and the
extracting the linear component of the transform.

-}
m22 : Fuzzer M.M22
m22 =
    affineTransform |> Fuzz.map M.affGetLinear


{-| Fuzzer for `P2`.
-}
p2 : Fuzzer M.P2
p2 =
    Fuzz.map M.v2p v2


{-| Fuzzer for `AffineTransform`.

Affine transforms are fuzzed by generating a sequence of affine transform
components and then forming the affine transform from their multiplication.

-}
affineTransform : Fuzzer M.AffineTransform
affineTransform =
    affineComponents |> Fuzz.map M.affFromComponents
