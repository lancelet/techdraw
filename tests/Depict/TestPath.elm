module Depict.TestPath exposing (suite)

import Depict.Anticipate as A
import Depict.Math as M
import Depict.Math.Fuzzer as MF
import Depict.Path as P
import Depict.Path.Compare as PC
import Depict.Path.Fuzzer as PF
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
