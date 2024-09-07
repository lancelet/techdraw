module Suite exposing (suite)

import Depict.TestMath
import Depict.TestPath
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Techdraw"
        [ Depict.TestMath.suite
        , Depict.TestPath.suite
        ]
