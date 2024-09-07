module Depict.Math.Compare exposing
    ( defaultTol, testClose
    , float, floatTol
    , angle2Pi, angle2PiTol
    , anglePi, anglePiTol
    , orientationPi, orientationPiTol
    , v2, v2Tol
    , n2, n2Tol
    , m22, m22Tol
    , p2, p2Tol
    , affineTransform, affineTransformTol
    )

{-| Comparison of mathematical types.

This module contains functions which compare the mathematical types and
produce `Anticipate` values.


# Utilities

@docs defaultTol, testClose


# Comparisons

@docs float, floatTol
@docs angle2Pi, angle2PiTol
@docs anglePi, anglePiTol
@docs orientationPi, orientationPiTol
@docs v2, v2Tol
@docs n2, n2Tol
@docs m22, m22Tol
@docs p2, p2Tol
@docs affineTransform, affineTransformTol

-}

import Debug
import Depict.Anticipate as Anticipate exposing (Anticipate)
import Depict.Math as M



---- Utility Functions --------------------------------------------------------


{-| Default tolerance when no explicit tolerance is given.
-}
defaultTol : M.Tol
defaultTol =
    M.TolAbsRel 1.0e-6 1.0e-6


{-| Convert a tolerance to a string value.
-}
reportTol : M.Tol -> String
reportTol tol =
    case tol of
        M.TolAbs tolAbs ->
            String.fromFloat tolAbs ++ " abs"

        M.TolRel tolRel ->
            String.fromFloat tolRel ++ " rel"

        M.TolAbsRel tolAbs tolRel ->
            String.fromFloat tolAbs
                ++ " abs || "
                ++ String.fromFloat tolRel
                ++ " rel"


{-| Produce a report string of a failure when two values are not close enough.
-}
reportFailClose : M.Tol -> a -> a -> String
reportFailClose tol a b =
    Debug.toString a
        ++ " /= "
        ++ Debug.toString b
        ++ " ("
        ++ reportTol tol
        ++ ")"


{-| Fail a "close" test.
-}
failClose : M.Tol -> a -> a -> Anticipate
failClose tol a b =
    reportFailClose tol a b |> Anticipate.fail


{-| Test a "close" condition.
-}
testClose : (M.Tol -> a -> a -> Bool) -> M.Tol -> a -> a -> Anticipate
testClose closeFn tol a b =
    if closeFn tol a b then
        Anticipate.pass

    else
        failClose tol a b



---- Comparisons --------------------------------------------------------------


{-| Check if two floating-point values are equal to default tolerance.
-}
float : Float -> Float -> Anticipate
float =
    floatTol defaultTol


{-| Check if two floating-point values are equal up to a tolerance.
-}
floatTol : M.Tol -> Float -> Float -> Anticipate
floatTol =
    testClose M.closeFloat


{-| Check if two `Angle2Pi` values are equal up to a default tolerance.
-}
angle2Pi : M.Angle2Pi -> M.Angle2Pi -> Anticipate
angle2Pi =
    angle2PiTol defaultTol


{-| Check if two `Angle2Pi` values are equal up to a tolerance.
-}
angle2PiTol : M.Tol -> M.Angle2Pi -> M.Angle2Pi -> Anticipate
angle2PiTol =
    testClose M.closeAngle2Pi


{-| Check if two `AnglePi` values are equal up to a default tolerance.
-}
anglePi : M.AnglePi -> M.AnglePi -> Anticipate
anglePi =
    anglePiTol defaultTol


{-| Check if two `AnglePi` values are equal up to a tolerance.
-}
anglePiTol : M.Tol -> M.AnglePi -> M.AnglePi -> Anticipate
anglePiTol =
    testClose M.closeAnglePi


{-| Check if two `OrientationPi` values are equal up to a default tolerance.
-}
orientationPi : M.OrientationPi -> M.OrientationPi -> Anticipate
orientationPi =
    orientationPiTol defaultTol


{-| Check if two `OrientationPi` values are equal up to a tolerance.
-}
orientationPiTol : M.Tol -> M.OrientationPi -> M.OrientationPi -> Anticipate
orientationPiTol =
    testClose M.closeOrientationPi


{-| Check if two `V2` values are equal up to a default tolerance.
-}
v2 : M.V2 -> M.V2 -> Anticipate
v2 =
    v2Tol defaultTol


{-| Check if two `V2` values are equal up to a tolerance.
-}
v2Tol : M.Tol -> M.V2 -> M.V2 -> Anticipate
v2Tol =
    testClose M.closeV2


{-| Check if two `N2` values are equal up to a default tolerance.
-}
n2 : M.N2 -> M.N2 -> Anticipate
n2 =
    n2Tol defaultTol


{-| Check if two `N2` values are equal up to a tolerance.
-}
n2Tol : M.Tol -> M.N2 -> M.N2 -> Anticipate
n2Tol =
    testClose M.closeN2


{-| Check if two `M22` values are equal up to a default tolerance.
-}
m22 : M.M22 -> M.M22 -> Anticipate
m22 =
    m22Tol defaultTol


{-| Check if two `M22` values are equal up to a tolerance.
-}
m22Tol : M.Tol -> M.M22 -> M.M22 -> Anticipate
m22Tol =
    testClose M.closeM22


{-| Check if two `P2` values are equal up to a default tolerance.
-}
p2 : M.P2 -> M.P2 -> Anticipate
p2 =
    p2Tol defaultTol


{-| Check if two `P2` values are equal up to a tolerance.
-}
p2Tol : M.Tol -> M.P2 -> M.P2 -> Anticipate
p2Tol =
    testClose M.closeP2


{-| Check if two `AffineTransform` values are equal up to a default tolerance.
-}
affineTransform : M.AffineTransform -> M.AffineTransform -> Anticipate
affineTransform =
    affineTransformTol defaultTol


{-| Check if two `AffineTransform` values are equal up to a tolerance.
-}
affineTransformTol :
    M.Tol
    -> M.AffineTransform
    -> M.AffineTransform
    -> Anticipate
affineTransformTol =
    testClose M.closeAffineTransform
