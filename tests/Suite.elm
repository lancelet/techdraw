module Suite exposing (suite)

import Techdraw.Internal.TestAffineMatrix
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Techdraw"
        [ Techdraw.Internal.TestAffineMatrix.suite ]
