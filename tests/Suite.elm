module Suite exposing (suite)

import Techdraw.TestMath
import Techdraw.TestPath
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Techdraw"
        [ Techdraw.TestMath.suite
        , Techdraw.TestPath.suite
        ]
