module Techdraw.TestMath exposing (suite)

import Fuzz
import Techdraw.Anticipate as A
import Techdraw.Math as M
import Techdraw.Math.Compare as MC
import Techdraw.Math.Fuzzer as MF
import Test exposing (Test)


suite : Test
suite =
    Test.describe "NewMath"
        [ suiteFloatingPoint
        , suiteFloatingPointComparisons
        , suiteAnglesAndOrientations
        , suiteAffineTransformationComponents
        , suiteAffineTransformations
        ]



---- Floating-point Function Tests --------------------------------------------


suiteFloatingPoint : Test
suiteFloatingPoint =
    Test.describe "Floating Point Functions" [ testSq ]


testSq : Test
testSq =
    A.fuzz (Fuzz.floatRange -100 100) "squaring a value" <|
        \x -> MC.float (M.sq x) (x ^ 2)



---- Comparison of Floating-Point Values Tests --------------------------------


suiteFloatingPointComparisons : Test
suiteFloatingPointComparisons =
    Test.describe "Comparison of Floating-Point Values"
        [ examplesCloseFloat
        , testAbsRel
        ]


examplesCloseFloat : Test
examplesCloseFloat =
    A.test "Examples of closeFloat" <|
        \() ->
            A.all
                [ A.true <| M.closeFloat (M.TolAbs 1) 0 0.9
                , A.false <| M.closeFloat (M.TolAbs 1) 0 1.1
                , A.true <| M.closeFloat (M.TolRel 1) 0 90
                , A.false <| M.closeFloat (M.TolRel 0.8) 0 90
                ]


testAbsRel : Test
testAbsRel =
    A.fuzz2 (Fuzz.floatRange -100 100)
        (Fuzz.floatRange -100 100)
        "TolAbsRel must match a check with either TolAbs or TolRel"
    <|
        \a b ->
            let
                absTol =
                    10

                relTol =
                    0.2

                absCheck =
                    M.closeFloat (M.TolAbs absTol) a b

                relCheck =
                    M.closeFloat (M.TolRel relTol) a b

                absRelCheck =
                    M.closeFloat (M.TolAbsRel absTol relTol) a b
            in
            A.true <| absRelCheck == (absCheck || relCheck)



---- Angles and Orientations Tests --------------------------------------------


suiteAnglesAndOrientations : Test
suiteAnglesAndOrientations =
    Test.describe "Angles and Orientations"
        [ examplesAngle2Pi
        , examplesAnglePi
        , examplesOrientationPi
        , testAngle2Pi
        , testAnglePi
        , testOrientationPi
        , examplesCloseAngle2Pi
        , examplesCloseAnglePi
        , examplesCloseOrientationPi
        ]


examplesAngle2Pi : Test
examplesAngle2Pi =
    A.test "Examples of Angle2Pi" <|
        \() ->
            [ ( 0, 0 )
            , ( pi / 2, pi / 2 )
            , ( 2 * pi, 0 )
            , ( -3 * pi / 2, pi / 2 )
            , ( 5 * pi / 2, pi / 2 )
            , ( -pi / 2, 3 * pi / 2 )
            , ( 7 * pi / 2, 3 * pi / 2 )
            ]
                |> List.map
                    (\( iVal, oVal ) ->
                        MC.float oVal (M.angle2Pi iVal |> M.getAngle2Pi)
                    )
                |> A.all


examplesAnglePi : Test
examplesAnglePi =
    A.test "Examples of AnglePi" <|
        \() ->
            [ ( 0, 0 )
            , ( pi / 2, pi / 2 )
            , ( 2 * pi, 0 )
            , ( -3 * pi / 2, pi / 2 )
            , ( 5 * pi / 2, pi / 2 )
            , ( -pi / 2, -pi / 2 )
            , ( 7 * pi / 2, -pi / 2 )
            ]
                |> List.map
                    (\( iVal, oVal ) ->
                        MC.float oVal (M.anglePi iVal |> M.getAnglePi)
                    )
                |> A.all


examplesOrientationPi : Test
examplesOrientationPi =
    A.test "Examples of OrientationPi" <|
        \() ->
            [ ( 0, 0 )
            , ( pi / 2, pi / 2 )
            , ( 2 * pi, 0 )
            , ( -3 * pi / 2, pi / 2 )
            , ( 5 * pi / 2, pi / 2 )
            , ( -pi / 2, pi / 2 )
            , ( 7 * pi / 2, pi / 2 )
            ]
                |> List.map
                    (\( iVal, oVal ) ->
                        MC.float
                            oVal
                            (M.orientationPi iVal |> M.getOrientationPi)
                    )
                |> A.all


testAngle2Pi : Test
testAngle2Pi =
    A.fuzz MF.floatWideAngle "Angle2Pi" <|
        \rawAngle ->
            let
                normAngle =
                    rawAngle |> M.angle2Pi |> M.getAngle2Pi
            in
            A.all
                [ MC.float (sin rawAngle) (sin normAngle)
                , MC.float (cos rawAngle) (cos normAngle)
                , A.atLeast 0 normAngle
                , A.lessThan (2 * pi) normAngle
                ]


testAnglePi : Test
testAnglePi =
    A.fuzz MF.floatWideAngle "AnglePi" <|
        \rawAngle ->
            let
                normAngle =
                    rawAngle |> M.anglePi |> M.getAnglePi
            in
            A.all
                [ MC.float (sin rawAngle) (sin normAngle)
                , MC.float (cos rawAngle) (cos normAngle)
                , A.greaterThan -pi normAngle
                , A.atMost pi normAngle
                ]


testOrientationPi : Test
testOrientationPi =
    A.fuzz MF.floatWideAngle "OrientationPi" <|
        \rawAngle ->
            let
                normAngle =
                    rawAngle |> M.orientationPi |> M.getOrientationPi
            in
            A.all
                [ MC.float (abs (sin rawAngle)) (abs (sin normAngle))
                , MC.float (abs (cos rawAngle)) (abs (cos normAngle))
                , MC.float (tan rawAngle) (tan normAngle)
                , A.atLeast 0 normAngle
                , A.lessThan pi normAngle
                ]


examplesCloseAngle2Pi : Test
examplesCloseAngle2Pi =
    A.test "Examples of closeAngle2Pi" <|
        \() ->
            A.all
                [ -- Angles near the middle of the range
                  A.true <|
                    M.closeAngle2Pi
                        (M.TolAbs 0.11)
                        (M.angle2Pi pi)
                        (M.angle2Pi (pi + 0.1))

                -- First low, second high
                , A.true <|
                    M.closeAngle2Pi
                        (M.TolAbs 0.11)
                        (M.angle2Pi 0)
                        (M.angle2Pi (2 * pi - 0.1))

                -- First high, second log
                , A.true <|
                    M.closeAngle2Pi
                        (M.TolAbs 0.11)
                        (M.angle2Pi (2 * pi - 0.1))
                        (M.angle2Pi 0)

                -- Unequal
                , A.false <|
                    M.closeAngle2Pi
                        (M.TolAbs 0.1)
                        (M.angle2Pi pi)
                        (M.angle2Pi (pi + 0.11))
                ]


examplesCloseAnglePi : Test
examplesCloseAnglePi =
    A.test "Examples of closeAnglePi" <|
        \() ->
            A.all
                [ -- Angles near the middle of the range
                  A.true <|
                    M.closeAnglePi
                        (M.TolAbs 0.11)
                        (M.anglePi 0)
                        (M.anglePi 0.1)

                -- First low, second high
                , A.true <|
                    M.closeAnglePi
                        (M.TolAbs 0.11)
                        (M.anglePi (-pi + 0.1))
                        (M.anglePi pi)

                -- First high, second low
                , A.true <|
                    M.closeAnglePi
                        (M.TolAbs 0.11)
                        (M.anglePi pi)
                        (M.anglePi (-pi + 0.1))

                -- Unequal
                , A.false <|
                    M.closeAnglePi
                        (M.TolAbs 0.1)
                        (M.anglePi 0)
                        (M.anglePi 0.11)
                ]


examplesCloseOrientationPi : Test
examplesCloseOrientationPi =
    A.test "Examples of closeOrientationPi" <|
        \() ->
            A.all
                [ -- Orientation near the middle of the range
                  A.true <|
                    M.closeOrientationPi
                        (M.TolAbs 0.11)
                        (M.orientationPi (pi / 2))
                        (M.orientationPi (pi / 2 + 0.1))

                -- First low, second high
                , A.true <|
                    M.closeOrientationPi
                        (M.TolAbs 0.11)
                        (M.orientationPi 0)
                        (M.orientationPi (pi - 0.1))

                -- First high, second low
                , A.true <|
                    M.closeOrientationPi
                        (M.TolAbs 0.11)
                        (M.orientationPi (pi - 0.1))
                        (M.orientationPi 0)

                -- Unequal
                , A.false <|
                    M.closeOrientationPi
                        (M.TolAbs 0.1)
                        (M.orientationPi (pi / 2))
                        (M.orientationPi (pi / 2 + 0.11))
                ]



---- Affine Transformation Components Tests -----------------------------------


suiteAffineTransformationComponents : Test
suiteAffineTransformationComponents =
    Test.describe "Affine Transformation Components"
        [ exampleP2ApplyRotation
        , exampleP2ApplyScaling
        , exampleP2ApplyShearingX
        , exampleP2ApplyTranslation
        , testRotationVectorMagnitudeInvariance
        , testRotationPointDistanceInvariance
        , testTranslationPointDistanceInvariance
        , testInvertRotation
        , testInvertScaling
        , testInvertShearingX
        , testInvertTranslation
        , testInvertAffineComponent
        ]


exampleP2ApplyRotation : Test
exampleP2ApplyRotation =
    A.test "Example of p2ApplyRotation" <|
        \() ->
            let
                p =
                    M.p2 1 0

                pExpect =
                    M.p2 0 1

                pActual =
                    M.p2ApplyRotation (M.Rotation <| M.angle2Pi (pi / 2)) p
            in
            MC.p2 pActual pExpect


exampleP2ApplyScaling : Test
exampleP2ApplyScaling =
    A.test "Example of p2ApplyScaling" <|
        \() ->
            let
                p =
                    M.p2 1 2

                pExpect =
                    M.p2 5 14

                pActual =
                    M.p2ApplyScaling (M.Scaling 5 7) p
            in
            MC.p2 pActual pExpect


exampleP2ApplyShearingX : Test
exampleP2ApplyShearingX =
    A.test "Example of p2ApplyShearingX" <|
        \() ->
            let
                p =
                    M.p2 3 5

                pExpect =
                    M.p2 13 5

                pActual =
                    M.p2ApplyShearingX (M.ShearingX 2) p
            in
            MC.p2 pActual pExpect


exampleP2ApplyTranslation : Test
exampleP2ApplyTranslation =
    A.test "Example of p2ApplyTranslation" <|
        \() ->
            let
                p =
                    M.p2 1 2

                pExpect =
                    M.p2 13 17

                pActual =
                    M.p2ApplyTranslation (M.Translation (M.v2 12 15)) p
            in
            MC.p2 pActual pExpect


testRotationVectorMagnitudeInvariance : Test
testRotationVectorMagnitudeInvariance =
    A.fuzz2 MF.p2
        MF.rotation
        "Vector magnitude is invariant under rotation"
    <|
        \p rotation ->
            M.p2ApplyRotation rotation p
                |> M.p2v
                |> M.v2Mag
                |> MC.float (M.v2Mag <| M.p2v p)


testRotationPointDistanceInvariance : Test
testRotationPointDistanceInvariance =
    A.fuzz3
        MF.p2
        MF.p2
        MF.rotation
        "Distance between points is invariant under rotation"
    <|
        \p1 p2 rotation ->
            let
                dBefore =
                    M.p2Distance p1 p2

                dAfter =
                    M.p2Distance
                        (M.p2ApplyRotation rotation p1)
                        (M.p2ApplyRotation rotation p2)
            in
            MC.float dBefore dAfter


testTranslationPointDistanceInvariance : Test
testTranslationPointDistanceInvariance =
    A.fuzz3
        MF.p2
        MF.p2
        MF.translation
        "Distance between points is invariant under translation"
    <|
        \p1 p2 translation ->
            let
                dBefore =
                    M.p2Distance p1 p2

                dAfter =
                    M.p2Distance
                        (M.p2ApplyTranslation translation p1)
                        (M.p2ApplyTranslation translation p2)
            in
            MC.float dBefore dAfter


testInvertRotation : Test
testInvertRotation =
    A.fuzz2 MF.p2 MF.rotation "invertRotation" <|
        \p rotation ->
            M.p2ApplyRotation rotation p
                |> M.p2ApplyRotation (M.invertRotation rotation)
                |> MC.p2 p


testInvertScaling : Test
testInvertScaling =
    A.fuzz2 MF.p2 MF.scaling "invertScaling" <|
        \p scaling ->
            M.p2ApplyScaling scaling p
                |> M.p2ApplyScaling (M.invertScaling scaling)
                |> MC.p2 p


testInvertShearingX : Test
testInvertShearingX =
    A.fuzz2 MF.p2 MF.shearingX "invertShearingX" <|
        \p shearingX ->
            M.p2ApplyShearingX shearingX p
                |> M.p2ApplyShearingX (M.invertShearingX shearingX)
                |> MC.p2 p


testInvertTranslation : Test
testInvertTranslation =
    A.fuzz2 MF.p2 MF.translation "invertTranslation" <|
        \p translation ->
            M.p2ApplyTranslation translation p
                |> M.p2ApplyTranslation (M.invertTranslation translation)
                |> MC.p2 p


testInvertAffineComponent : Test
testInvertAffineComponent =
    A.fuzz2 MF.p2 MF.affineComponent "invertAffineComponent" <|
        \p component ->
            M.p2ApplyAffineComponent component p
                |> M.p2ApplyAffineComponent (M.invertAffineComponent component)
                |> MC.p2 p



---- Affine Transformations Tests ---------------------------------------------


suiteAffineTransformations : Test
suiteAffineTransformations =
    Test.describe "Affine Transformations"
        [ testAffIdentity
        , testAffRotation
        , testAffScaling
        , testAffShearingX
        , testAffTranslation
        , testAffFromComponent
        , testAffineComponentsMatchMatrix
        , testAffInvert
        ]


testAffIdentity : Test
testAffIdentity =
    A.fuzz MF.p2 "affIdentity" <|
        \p ->
            MC.p2 p (M.p2ApplyAffineTransform M.affIdentity p)


testAffRotation : Test
testAffRotation =
    A.fuzz2 MF.p2 MF.rotation "affRotation" <|
        \p rotation ->
            MC.p2
                (M.p2ApplyRotation rotation p)
                (M.p2ApplyAffineTransform (M.affRotation rotation) p)


testAffScaling : Test
testAffScaling =
    A.fuzz2 MF.p2 MF.scaling "affScaling" <|
        \p scaling ->
            MC.p2
                (M.p2ApplyScaling scaling p)
                (M.p2ApplyAffineTransform (M.affScaling scaling) p)


testAffShearingX : Test
testAffShearingX =
    A.fuzz2 MF.p2 MF.shearingX "affShearingX" <|
        \p shearingX ->
            MC.p2
                (M.p2ApplyShearingX shearingX p)
                (M.p2ApplyAffineTransform (M.affShearingX shearingX) p)


testAffTranslation : Test
testAffTranslation =
    A.fuzz2 MF.p2 MF.translation "affTranslation" <|
        \p translation ->
            MC.p2
                (M.p2ApplyTranslation translation p)
                (M.p2ApplyAffineTransform (M.affTranslation translation) p)


testAffFromComponent : Test
testAffFromComponent =
    A.fuzz2 MF.p2 MF.affineComponent "affFromComponent" <|
        \p component ->
            MC.p2
                (M.p2ApplyAffineComponent component p)
                (M.p2ApplyAffineTransform (M.affFromComponent component) p)


testAffineComponentsMatchMatrix : Test
testAffineComponentsMatchMatrix =
    A.fuzz2
        MF.p2
        MF.affineComponents
        "Applying a single affine transform matches a sequence of components"
    <|
        \p components ->
            MC.p2
                (M.p2ApplyAffineTransform (M.affFromComponents components) p)
                (List.foldl M.p2ApplyAffineComponent p components)


testAffInvert : Test
testAffInvert =
    A.fuzz2 MF.p2 MF.affineTransform "affInvert" <|
        \p affineTransform ->
            M.p2ApplyAffineTransform affineTransform p
                |> M.p2ApplyAffineTransform (M.affInvert affineTransform)
                |> MC.p2 p
