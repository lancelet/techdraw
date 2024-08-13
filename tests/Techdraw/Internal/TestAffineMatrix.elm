module Techdraw.Internal.TestAffineMatrix exposing (suite)

import Fuzz exposing (floatRange)
import Techdraw.Internal.AffineMatrix as AffineMatrix
import Techdraw.Internal.Expect exposing (hptEqual, matEqual, ptEqual)
import Techdraw.Internal.Fuzzer
    exposing
        ( Range(..)
        , TestTransform(..)
        , TestTranslation(..)
        , applyTestTransforms
        , genAffineMatrix
        , genAffineMatrixTestTransforms
        , genHPt
        , genPt
        , genPtRange
        , toAffineMatrix
        )
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "AffineMatrix"
        [ testIdentityPtMul
        , testIdentityHPtMul
        , testTranslatePt
        , testScalePt
        , testRotatePt
        , testTransRot
        , testMulIdentityTranslate
        , testMulIdentityRotate
        , testMulComponents
        , testTestTransformTranslate
        , testTestTransformsTranslateApply
        , testMulInverse
        , testInversePt
        ]


testIdentityPtMul : Test
testIdentityPtMul =
    fuzz genPt "Multiplying an identity matrix by a point gives the same point" <|
        \pt -> ptEqual pt (AffineMatrix.mulPt AffineMatrix.identity pt)


testIdentityHPtMul : Test
testIdentityHPtMul =
    fuzz genHPt "Multiplying an identity matrix by an hpoint gives the same hpoint" <|
        \hpt -> hptEqual hpt (AffineMatrix.mulHVec AffineMatrix.identity hpt)


testTranslatePt : Test
testTranslatePt =
    fuzz2 genPt genPt "Translating a point actually translates it" <|
        \( px, py ) ( tx, ty ) ->
            let
                mat =
                    AffineMatrix.translation ( tx, ty )

                expected =
                    ( px + tx, py + ty )

                actual =
                    AffineMatrix.mulPt mat ( px, py )
            in
            ptEqual expected actual


testScalePt : Test
testScalePt =
    fuzz2
        (genPtRange (Range { min = 0.1, max = 5.0 }))
        genPt
        "Scaling a point actually scales it"
    <|
        \( sx, sy ) ( px, py ) ->
            let
                mat =
                    AffineMatrix.scaling ( sx, sy )

                expected =
                    ( sx * px, sy * py )

                actual =
                    AffineMatrix.mulPt mat ( px, py )
            in
            ptEqual expected actual


testRotatePt : Test
testRotatePt =
    fuzz2 (floatRange (-8 * pi) (8 * pi)) genPt "Rotating a point actually rotates it" <|
        \angleRad ( px, py ) ->
            let
                mat =
                    AffineMatrix.rotationRadians angleRad

                c =
                    cos angleRad

                s =
                    sin angleRad

                expected =
                    ( c * px - s * py, s * px + c * py )

                actual =
                    AffineMatrix.mulPt mat ( px, py )
            in
            ptEqual expected actual


testTransRot : Test
testTransRot =
    fuzz3 genPt
        (floatRange (-8 * pi) (8 * pi))
        genPt
        "Translation then rotation is ordered correctly"
    <|
        \( tx, ty ) angleRad ( px, py ) ->
            let
                -- Note that here, the first operation is translation, and the second
                -- operation is rotation.
                mat =
                    AffineMatrix.mul
                        (AffineMatrix.rotationRadians angleRad)
                        (AffineMatrix.translation ( tx, ty ))

                c =
                    cos angleRad

                s =
                    sin angleRad

                -- Translate first.
                ptx =
                    px + tx

                pty =
                    py + ty

                -- Then rotate the translated coordinates.
                expected =
                    ( c * ptx - s * pty, s * ptx + c * pty )

                actual =
                    AffineMatrix.mulPt mat ( px, py )
            in
            ptEqual expected actual


testMulIdentityTranslate : Test
testMulIdentityTranslate =
    fuzz
        genPt
        "Multiplying a translation matrix by identity gives the original matrix"
    <|
        \pt ->
            let
                expected =
                    AffineMatrix.translation pt

                mat =
                    AffineMatrix.mul expected AffineMatrix.identity
            in
            matEqual expected mat


testMulIdentityRotate : Test
testMulIdentityRotate =
    fuzz
        (floatRange (-5 * pi) (5 * pi))
        "Multiplying a rotation matrix by identity gives the original matrix"
    <|
        \radAngle ->
            let
                expected =
                    AffineMatrix.rotationRadians radAngle

                mat =
                    AffineMatrix.mul expected AffineMatrix.identity
            in
            matEqual expected mat


testMulComponents : Test
testMulComponents =
    fuzz2
        genPt
        genAffineMatrixTestTransforms
        "Multiplying by a matrix must match individual transformations"
    <|
        \pt ( mat, testTransforms ) ->
            let
                expected =
                    applyTestTransforms testTransforms pt

                actual =
                    AffineMatrix.mulPt mat pt
            in
            ptEqual expected actual


testTestTransformTranslate : Test
testTestTransformTranslate =
    test "Check that a test transforms Translate is encoded correctly." <|
        \() ->
            let
                tt =
                    TTTranslate (TestTranslation { tx = 300, ty = 200 })

                expected =
                    AffineMatrix.new 1 0 0 1 300 200

                actual =
                    toAffineMatrix tt
            in
            matEqual expected actual


testTestTransformsTranslateApply : Test
testTestTransformsTranslateApply =
    fuzz2 genPt genPt "Check that a test transforms Translate is applied correctly" <|
        \( px, py ) ( tx, ty ) ->
            let
                tt =
                    TTTranslate (TestTranslation { tx = tx, ty = ty })

                expected =
                    ( px + tx, py + ty )

                actual =
                    applyTestTransforms [ tt ] ( px, py )
            in
            ptEqual expected actual


testMulInverse : Test
testMulInverse =
    fuzz genAffineMatrix "A matrix multiplied by its inverse is identity" <|
        \mat ->
            let
                inv =
                    AffineMatrix.invert mat

                actual =
                    AffineMatrix.mul mat inv
            in
            matEqual AffineMatrix.identity actual


testInversePt : Test
testInversePt =
    fuzz2 genPt
        genAffineMatrix
        "Reversing a transformation produces the original point"
    <|
        \pt fwdMat ->
            let
                invMat =
                    AffineMatrix.invert fwdMat

                fwdPt =
                    AffineMatrix.mulPt fwdMat pt

                invPt =
                    AffineMatrix.mulPt invMat fwdPt
            in
            ptEqual invPt pt
