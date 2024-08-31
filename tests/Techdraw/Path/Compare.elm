module Techdraw.Path.Compare exposing
    ( defaultTol
    , path, pathTol
    )

{-| Comparison of path types.

This module contains functions which compare the path types and produce
`Anticipate` values.

@docs defaultTol
@docs path, pathTol

-}

import Techdraw.Anticipate exposing (Anticipate)
import Techdraw.Math as M
import Techdraw.Math.Compare exposing (testClose)
import Techdraw.Path as P


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
