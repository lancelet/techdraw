module Techdraw.Internal.TestBiTransform exposing (suite)

import Techdraw.Internal.AffineMatrix as AffineMatrix
import Techdraw.Internal.BiTransform as BiTransform
    exposing
        ( BiTransform(..)
        , getBwd
        )
import Techdraw.Internal.Expect exposing (matEqual)
import Techdraw.Internal.Fuzzer
    exposing
        ( TestRotate(..)
        , TestScale(..)
        , TestTransform(..)
        , TestTranslation(..)
        , genRotate
        , genScale
        , genTranslate
        , toAffineMatrix
        )
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "BiTransform"
        [ testTranslateInverse
        , testScaleInverse
        , testRotateInverse
        ]


testTranslateInverse : Test
testTranslateInverse =
    fuzz
        genTranslate
        "Check that inverse translation matches a general inverse calculation"
    <|
        \(TestTranslation t) ->
            let
                bt =
                    BiTransform.translation ( t.tx, t.ty )

                mat =
                    toAffineMatrix (TTTranslate (TestTranslation t))

                inv =
                    AffineMatrix.invert mat
            in
            matEqual inv (getBwd bt)


testScaleInverse : Test
testScaleInverse =
    fuzz
        genScale
        "Check that inverse scale matches a general inverse calculation"
    <|
        \(TestScale s) ->
            let
                bt =
                    BiTransform.scaling ( s.sx, s.sy )

                mat =
                    toAffineMatrix (TTScale (TestScale s))

                inv =
                    AffineMatrix.invert mat
            in
            matEqual inv (getBwd bt)


testRotateInverse : Test
testRotateInverse =
    fuzz
        genRotate
        "Check that inverse rotation matches a general inverse calculation"
    <|
        \(TestRotate r) ->
            let
                bt =
                    BiTransform.rotationRadians r.radians

                mat =
                    toAffineMatrix (TTRotate (TestRotate r))

                inv =
                    AffineMatrix.invert mat
            in
            matEqual inv (getBwd bt)
