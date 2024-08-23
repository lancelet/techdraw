module Techdraw.TestMath exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, floatRange)
import Techdraw.Anticipate
    exposing
        ( all
        , atLeast
        , atMost
        , fuzz
        , fuzz2
        , fuzz3
        , greaterThan
        , lessThan
        , pass
        , within
        )
import Techdraw.Math as Math
import Techdraw.Math.Compare as MathCompare
import Techdraw.Math.Fuzzer as MathFuzzer
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Math"
        [ angleTests
        , matVecTests
        , affineTransformTests
        , pathTests
        , ellipseOpTests
        ]



---- Angle Tests --------------------------------------------------------------


angleTests : Test
angleTests =
    describe "Angles" [ angle2PiTest, anglePiTest, orientationPiTest ]


angleTol : FloatingPointTolerance
angleTol =
    AbsoluteOrRelative 1.0e-6 1.0e-6


genFloatAngle : Fuzzer Float
genFloatAngle =
    floatRange (-10 * pi) (10 * pi)


angle2PiTest : Test
angle2PiTest =
    fuzz genFloatAngle "Angle2Pi properties" <|
        \floatAngle ->
            let
                angle2pi =
                    Math.angle2Pi floatAngle
            in
            all
                [ within angleTol (sin floatAngle) (sin <| Math.getAngle2Pi angle2pi)
                , within angleTol (cos floatAngle) (cos <| Math.getAngle2Pi angle2pi)
                , atLeast 0 (Math.getAngle2Pi angle2pi)
                , lessThan (2 * pi) (Math.getAngle2Pi angle2pi)
                ]


anglePiTest : Test
anglePiTest =
    fuzz genFloatAngle "AnglePi properties" <|
        \floatAngle ->
            let
                anglePi =
                    Math.anglePi floatAngle
            in
            all
                [ within angleTol (sin floatAngle) (sin <| Math.getAnglePi anglePi)
                , within angleTol (cos floatAngle) (cos <| Math.getAnglePi anglePi)
                , greaterThan -pi (Math.getAnglePi anglePi)
                , atMost pi (Math.getAnglePi anglePi)
                ]


orientationPiTest : Test
orientationPiTest =
    fuzz genFloatAngle "OrientationPi properties" <|
        \floatAngle ->
            let
                orientationPi =
                    Math.orientationPi floatAngle
            in
            all
                [ -- sin may differ by sign
                  within
                    angleTol
                    (abs <| sin floatAngle)
                    (abs <| sin <| Math.getOrientationPi orientationPi)
                , -- cos may differ by a sign
                  within
                    angleTol
                    (abs <| cos floatAngle)
                    (abs <| cos <| Math.getOrientationPi orientationPi)
                , -- check range
                  atLeast 0 (Math.getOrientationPi orientationPi)
                , lessThan pi (Math.getOrientationPi orientationPi)
                ]



---- Matrix and Vector Tests --------------------------------------------------


matVecTests : Test
matVecTests =
    describe "Matrices and Vectors"
        [ m22InvIdentityTest
        , m22InvTransformTest
        , m22UniformScaleVec2Test
        , m22RotVecsTest
        ]


m22InvIdentityTest : Test
m22InvIdentityTest =
    fuzz MathFuzzer.m22_affine "Inverse of a matrix: (M * M^-1) = (M^-1 * M) = I" <|
        \m ->
            let
                minv =
                    Math.m22Inv m
            in
            all
                [ MathCompare.m22 Math.m22Identity (Math.m22Mul m minv)
                , MathCompare.m22 Math.m22Identity (Math.m22Mul minv m)
                ]


m22InvTransformTest : Test
m22InvTransformTest =
    fuzz2 MathFuzzer.v2
        MathFuzzer.m22_affine
        "Inverse transform of a point: (M ^ -1) * (M * V) = V"
    <|
        \v m ->
            let
                minv =
                    Math.m22Inv m

                tol =
                    AbsoluteOrRelative 1.0e-5 1.0e-5
            in
            MathCompare.v2_tol tol v (Math.m22v2Mul minv (Math.m22v2Mul m v))


m22UniformScaleVec2Test : Test
m22UniformScaleVec2Test =
    fuzz3
        (Fuzz.oneOf [ floatRange -10 -0.05, floatRange 0.05 10 ])
        MathFuzzer.v2
        MathFuzzer.v2
        "Uniformly scaling two vectors keeps their normals and scales distance between them."
    <|
        \scale v1 v2 ->
            let
                sign =
                    if scale > 0 then
                        1

                    else
                        -1

                n1 =
                    Math.v2Norm v1

                n2 =
                    Math.v2Norm v2

                d12 =
                    Math.p2Distance (Math.v2p v1) (Math.v2p v2)

                m_uniformScale =
                    Math.m22Scale (Math.Scale scale scale)

                v1_prime =
                    Math.m22v2Mul m_uniformScale v1

                v2_prime =
                    Math.m22v2Mul m_uniformScale v2

                n1_prime =
                    Math.v2Scale sign (Math.v2Norm v1_prime)

                n2_prime =
                    Math.v2Scale sign (Math.v2Norm v2_prime)

                d12_prime =
                    Math.p2Distance (Math.v2p v1_prime) (Math.v2p v2_prime)

                tol =
                    AbsoluteOrRelative 1.0e-6 1.0e-6
            in
            all
                [ within tol (sign * scale * d12) d12_prime

                -- There can be NaNs due to zero-length v1
                , if not (Math.v2ContainsNaN n1) then
                    MathCompare.v2 n1 n1_prime

                  else
                    pass

                -- There can be NaNs due to zero-length v2
                , if not (Math.v2ContainsNaN n2) then
                    MathCompare.v2 n2 n2_prime

                  else
                    pass
                ]


m22RotVecsTest : Test
m22RotVecsTest =
    fuzz3
        MathFuzzer.angle2Pi
        MathFuzzer.v2
        MathFuzzer.v2
        "Rotating two points keeps the distance between them and their lengths unchanged."
    <|
        \angle v1 v2 ->
            let
                len_v1 =
                    Math.v2Mag v1

                len_v2 =
                    Math.v2Mag v2

                d12 =
                    Math.p2Distance (Math.v2p v1) (Math.v2p v2)

                m_rot =
                    Math.m22Rotation (Math.Rotation angle)

                v1_prime =
                    Math.m22v2Mul m_rot v1

                v2_prime =
                    Math.m22v2Mul m_rot v2

                len_v1_prime =
                    Math.v2Mag v1_prime

                len_v2_prime =
                    Math.v2Mag v2_prime

                d12_prime =
                    Math.p2Distance (Math.v2p v1_prime) (Math.v2p v2_prime)

                tol =
                    AbsoluteOrRelative 1.0e-5 1.0e-5
            in
            all
                [ within tol len_v1 len_v1_prime
                , within tol len_v2 len_v2_prime
                , within tol d12 d12_prime
                ]



---- AffineTransform tests ---------------------------------------------------


affineTransformTests : Test
affineTransformTests =
    describe "AffineTransform"
        [ atIdentityTest
        , atTranslateTest
        , atRotateTest
        , atScaleTest
        , atShearTest
        , atInvTranslateTest
        , atInvRotateTest
        , atInvScaleTest
        , atInvShearTest
        , atGenInvTest
        , atComponentsTest
        ]


atIdentityTest : Test
atIdentityTest =
    fuzz MathFuzzer.p2 "I * v = v for all v" <|
        \p ->
            MathCompare.p2
                p
                (Math.affApplyP2 Math.affIdentity p)


atTranslateTest : Test
atTranslateTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.translation
        "Translation: Isolated translation agrees with matrix multiply"
    <|
        \p t ->
            MathCompare.p2
                (Math.applyTranslation t p)
                (Math.affApplyP2 (Math.affTranslate t) p)


atRotateTest : Test
atRotateTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.rotation
        "Rotation: Isolated rotation agrees with matrix multiply"
    <|
        \p r ->
            MathCompare.p2
                (Math.applyRotation r p)
                (Math.affApplyP2 (Math.affRotate r) p)


atScaleTest : Test
atScaleTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.scale
        "Scale: Isolated scale agrees with matrix multiply"
    <|
        \p s ->
            MathCompare.p2
                (Math.applyScale s p)
                (Math.affApplyP2 (Math.affScale s) p)


atShearTest : Test
atShearTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.shearx
        "ShearX: Isolated shearx agrees with matrix multiply"
    <|
        \p h ->
            MathCompare.p2
                (Math.applyShearX h p)
                (Math.affApplyP2 (Math.affShearX h) p)


atInvTranslateTest : Test
atInvTranslateTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.translation
        "Translation: Inverted translation agrees with matrix multiply"
    <|
        \p t ->
            MathCompare.p2
                (Math.applyTranslation (Math.invertTranslation t) p)
                (Math.affApplyP2 (Math.affInvert (Math.affTranslate t)) p)


atInvRotateTest : Test
atInvRotateTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.rotation
        "Rotation: Inverted rotation agrees with matrix multiply"
    <|
        \p r ->
            MathCompare.p2
                (Math.applyRotation (Math.invertRotation r) p)
                (Math.affApplyP2 (Math.affInvert (Math.affRotate r)) p)


atInvScaleTest : Test
atInvScaleTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.scale
        "Scale: Inverted scale agrees with matrix multiply"
    <|
        \p s ->
            MathCompare.p2_tol (AbsoluteOrRelative 1.0e-5 1.0e-5)
                (Math.applyScale (Math.invertScale s) p)
                (Math.affApplyP2 (Math.affInvert (Math.affScale s)) p)


atInvShearTest : Test
atInvShearTest =
    fuzz2
        MathFuzzer.p2
        MathFuzzer.shearx
        "ShearX: Inverted shear agrees with matrix multiply"
    <|
        \p h ->
            MathCompare.p2
                (Math.applyShearX (Math.invertShearX h) p)
                (Math.affApplyP2 (Math.affInvert (Math.affShearX h)) p)


atGenInvTest : Test
atGenInvTest =
    fuzz2
        MathFuzzer.affineTransform
        MathFuzzer.p2
        "Inverse of a general affine transform - properties"
    <|
        \m p ->
            let
                invm =
                    Math.affInvert m

                idty =
                    Math.affIdentity

                atol =
                    AbsoluteOrRelative 1.0e-3 1.0e-3

                vtol =
                    AbsoluteOrRelative 1.0e-2 1.0e-2
            in
            all
                [ -- Pre- and post-multiplying the inverse must be identity
                  MathCompare.affineTransform_tol atol idty (Math.affMul m invm)
                , MathCompare.affineTransform_tol atol idty (Math.affMul invm m)

                -- Forward and inverse operations must transform a point back to itself
                , MathCompare.p2_tol vtol p (Math.affApplyP2 invm (Math.affApplyP2 m p))
                , MathCompare.p2_tol vtol p (Math.affApplyP2 m (Math.affApplyP2 invm p))
                ]


{-| Compare separate transform components to an assembled AffineTransform.

In this test, we generate a list of individual affine transform components
(ie. translation, rotation, scale and shear). We then assemble an
AffineTransform from those components, and ensure that, when applied to
points, the assembled transform does the same thing as the individual
transform components applied in sequence.

-}
atComponentsTest : Test
atComponentsTest =
    fuzz2
        (Fuzz.listOfLengthBetween 1 10 MathFuzzer.affineTransformComponent)
        MathFuzzer.p2
        "AffineTransform does the same thing as its separate components"
    <|
        \components p ->
            let
                -- Assemble a single AffineTransform by multiplying the
                -- affine transforms of each component in a foldl.
                m =
                    List.foldl
                        (\t2 t1 -> Math.affMul (Math.affFromComponent t2) t1)
                        Math.affIdentity
                        components

                -- Apply the individual transform components in a foldl
                -- to compute a transformed point.
                pc =
                    List.foldl
                        (\atc pp -> Math.applyAffineTransformComponent atc pp)
                        p
                        components

                -- Apply the single AffineTransform.
                pm =
                    Math.affApplyP2 m p
            in
            MathCompare.p2 pc pm



---- Path tests ---------------------------------------------------------------


pathTests : Test
pathTests =
    describe "Paths"
        [ subPathCurvesRoundTripTest
        , pathTransformRoundTripTest
        ]


subPathCurvesRoundTripTest : Test
subPathCurvesRoundTripTest =
    fuzz
        (MathFuzzer.subPathNormalized (MathFuzzer.MinEcc 0.05))
        "Round-trip a normalized subpath to joined curves and back"
    <|
        \subpath ->
            let
                joinedCurves =
                    Math.subPathToJoinedCurves subpath

                finalSubPath =
                    Math.joinedCurvesToSubPath
                        (Math.subPathClosure subpath)
                        joinedCurves

                anticipation =
                    MathCompare.subPath subpath finalSubPath
            in
            anticipation


pathTransformRoundTripTest : Test
pathTransformRoundTripTest =
    fuzz2
        (MathFuzzer.pathNormalized (MathFuzzer.MinEcc 0.3))
        MathFuzzer.affineTransform
        "Round-trip a normalized subpath through a transformation and back"
    <|
        \initPath transform ->
            let
                tol =
                    AbsoluteOrRelative 1.0e-2 1.0e-2

                xfPath =
                    Math.affApplyPath transform initPath

                finalPath =
                    Math.affApplyPath (Math.affInvert transform) xfPath
            in
            MathCompare.path_tol tol initPath finalPath



---- Ellipse Operations Tests -------------------------------------------------


ellipseOpTests : Test
ellipseOpTests =
    describe "Ellipse Operations"
        [ ellipseRadiiToBiasedAxesTest
        , ellipseImplicitsMatchEllipseTest
        , ellipseAxesImplicitsRoundTrip
        ]


ellipseRadiiToBiasedAxesTest : Test
ellipseRadiiToBiasedAxesTest =
    fuzz3
        (Fuzz.oneOf [ floatRange -100 -1, floatRange 1 100 ])
        (Fuzz.oneOf [ floatRange -100 -1, floatRange 1 100 ])
        MathFuzzer.orientationPi
        "Properties of ellipseRadiiToBiasedAxes function"
    <|
        \rx ry xOrient ->
            let
                tol =
                    AbsoluteOrRelative 1.0e-6 1.0e-6

                { semiMajor, semiMinor, theta } =
                    Math.ellipseRadiiToBiasedAxes { rx = rx, ry = ry, xOrient = xOrient }

                reXOrient =
                    if abs (abs rx - semiMajor) < abs (abs ry - semiMajor) then
                        theta

                    else
                        theta
                            |> Math.getOrientationPi
                            |> (\xx -> xx + pi / 2)
                            |> Math.orientationPi
            in
            all
                [ greaterThan 0 semiMinor
                , greaterThan 0 semiMajor
                , atLeast semiMinor semiMajor
                , within tol semiMajor <| max (abs rx) (abs ry)
                , within tol semiMinor <| min (abs rx) (abs ry)
                , MathCompare.orientationPi xOrient reXOrient
                ]


ellipseImplicitsMatchEllipseTest : Test
ellipseImplicitsMatchEllipseTest =
    fuzz
        (Fuzz.map4
            (\rx ry xOrient angleParam ->
                { rx = rx
                , ry = ry
                , xOrient = xOrient
                , angleParam = angleParam
                }
            )
            (floatRange 1 100)
            (floatRange 1 100)
            MathFuzzer.orientationPi
            MathFuzzer.angle2Pi
        )
        "Ellipse implicits match the ellipse equation"
    <|
        \{ rx, ry, xOrient, angleParam } ->
            let
                tol =
                    AbsoluteOrRelative 1.0e-5 1.0e-5

                -- Find ellipse implicit parameters.
                { a, b, c } =
                    { rx = rx, ry = ry, xOrient = xOrient }
                        |> Math.ellipseRadiiToBiasedAxes
                        |> Math.ellipseImplicits

                xOrientAngle =
                    xOrient |> Math.getOrientationPi |> Math.angle2Pi

                angleParamF =
                    angleParam |> Math.getAngle2Pi

                -- Find a point on the ellipse.
                -- This equation is from the SVG implementors notes, as the
                -- parameterized matrix equation of an ellipse:
                -- https://www.w3.org/TR/SVG/implnote.html#ArcParameterizationAlternatives
                (Math.V2 v) =
                    Math.m22v2Mul
                        (Math.m22Rotation <| Math.Rotation xOrientAngle)
                        (Math.v2 (rx * cos angleParamF) (ry * sin angleParamF))

                -- Evaluate the ellipse implicit equation LHS
                z =
                    (a * Math.sq v.e1) + (b * v.e1 * v.e2) + (c * Math.sq v.e2) - 1
            in
            within tol 0 z


ellipseAxesImplicitsRoundTrip : Test
ellipseAxesImplicitsRoundTrip =
    fuzz3
        (Fuzz.oneOf [ floatRange -100 -1, floatRange 1 100 ])
        (Fuzz.oneOf [ floatRange -100 -1, floatRange 1 100 ])
        MathFuzzer.orientationPi
        "Round-trip an ellipse from axis form to implicits and back"
    <|
        \rx ry xOrient ->
            let
                min_eccentricity =
                    0.05

                -- We want to avoid ellipses that are too close to circular for
                -- this test, since as ellipses approach circles, the major and
                -- minor axis directions become meaningless. So, first we compute
                -- the eccentricity.
                axes0 =
                    { rx = rx, ry = ry, xOrient = xOrient } |> Math.ellipseRadiiToBiasedAxes

                eccentricity =
                    sqrt <| 1 - Math.sq axes0.semiMinor / Math.sq axes0.semiMajor

                -- Correct radii if the ellipse is too close to circular.
                ( rxx, ryy ) =
                    if eccentricity < min_eccentricity then
                        ( rx, 1.5 * rx )

                    else
                        ( rx, ry )

                tol =
                    AbsoluteOrRelative 1.0e-5 1.0e-5

                -- Normalize into the initial axis format.
                initAxis =
                    { rx = rxx, ry = ryy, xOrient = xOrient } |> Math.ellipseRadiiToBiasedAxes

                -- Convert to implicit parameters.
                implicitParams =
                    Math.ellipseImplicits initAxis

                -- Convert back to axis format.
                finalAxis =
                    Math.ellipseImplicitsToAxes implicitParams
            in
            all
                [ within tol initAxis.semiMajor finalAxis.semiMajor
                , within tol initAxis.semiMinor finalAxis.semiMinor
                , MathCompare.orientationPi_tol tol initAxis.theta finalAxis.theta
                ]
