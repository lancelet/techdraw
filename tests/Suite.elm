module Suite exposing (suite)

import Techdraw.TestMath
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Techdraw"
        [ Techdraw.TestMath.suite ]
