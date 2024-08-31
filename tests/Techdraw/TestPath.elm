module Techdraw.TestPath exposing (suite)

import Techdraw.Anticipate as A
import Techdraw.Math as M
import Techdraw.Math.Fuzzer as MF
import Techdraw.Path as P
import Techdraw.Path.Compare as PC
import Techdraw.Path.Fuzzer as PF
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Path"
        [ testPathApplyAffineTransformRoundtrip
        ]


testPathApplyAffineTransformRoundtrip : Test
testPathApplyAffineTransformRoundtrip =
    A.fuzz2
        MF.affineTransform
        (PF.pathNormalizedRadii (PF.MinRadius 1.2) (PF.MinEccentricity 0.25))
        "Round-trip path affine transformation"
    <|
        \transform path ->
            path
                |> P.pathApplyAffineTransform transform
                |> P.pathApplyAffineTransform (M.affInvert transform)
                |> PC.path path
