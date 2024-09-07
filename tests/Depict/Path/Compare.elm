module Depict.Path.Compare exposing
    ( defaultTol
    , path, pathTol
    )

{-| Comparison of path types.

This module contains functions which compare the path types and produce
`Anticipate` values.

@docs defaultTol
@docs path, pathTol

-}

import Depict.Anticipate exposing (Anticipate)
import Depict.Math as M
import Depict.Math.Compare exposing (testClose)
import Depict.Path as P


{-| Default tolerance when no explicit tolerance is given.
-}
defaultTol : M.Tol
defaultTol =
    M.TolAbsRel 1.0e-2 1.0e-3


{-| Check if two `Path` values are equal up to default tolerance.
-}
path : P.Path -> P.Path -> Anticipate
path =
    pathTol defaultTol


{-| Check if two `Path` values are equal up to a tolerance.
-}
pathTol : M.Tol -> P.Path -> P.Path -> Anticipate
pathTol =
    testClose P.closePath
