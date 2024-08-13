module Techdraw.Internal.Expect exposing
    ( defaultTol
    , ptEqual, ptEqualTol, hptEqual, hptEqualTol, matEqual, matEqualTol
    )

{-| Custom comparisons used in testing.


# Defaults

@docs defaultTol


# Functions

@docs ptEqual, ptEqualTol, hptEqual, hptEqualTol, matEqual, matEqualTol

-}

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Techdraw.Internal.AffineMatrix exposing (AffineMatrix(..))



---- Defaults -------------------------------------------------------------------------------------


{-| Default tolerance for comparisons.
-}
defaultTol : FloatingPointTolerance
defaultTol =
    AbsoluteOrRelative 1.0e-6 1.0e-6



---- Compare With Extraction ----------------------------------------------------------------------


{-| Compare a part of a structure (extracted using a function) up to a floating-point tolerance.
-}
cmpWithin : FloatingPointTolerance -> (a -> Float) -> a -> a -> Expectation
cmpWithin tol extract expected actual =
    Expect.within tol (extract expected) (extract actual)



---- Compare Points -------------------------------------------------------------------------------


{-| Compare two points using default tolerance.
-}
ptEqual : ( Float, Float ) -> ( Float, Float ) -> Expectation
ptEqual =
    ptEqualTol defaultTol


{-| Compare two points up to a tolerance.
-}
ptEqualTol : FloatingPointTolerance -> ( Float, Float ) -> ( Float, Float ) -> Expectation
ptEqualTol tol expected actual =
    Expect.all
        [ cmpWithin tol (\( x, _ ) -> x) expected
        , cmpWithin tol (\( _, y ) -> y) expected
        ]
        actual


{-| Compare two homogeneous points using default tolerance.
-}
hptEqual : ( Float, Float, Float ) -> ( Float, Float, Float ) -> Expectation
hptEqual =
    hptEqualTol defaultTol


{-| Compare two homogeneous points up to a tolerance.
-}
hptEqualTol :
    FloatingPointTolerance
    -> ( Float, Float, Float )
    -> ( Float, Float, Float )
    -> Expectation
hptEqualTol tol expected actual =
    Expect.all
        [ cmpWithin tol (\( x, _, _ ) -> x) expected
        , cmpWithin tol (\( _, y, _ ) -> y) expected
        , cmpWithin tol (\( _, _, w ) -> w) expected
        ]
        actual



---- Compare Affine Matrices ----------------------------------------------------------------------


{-| Compare two matrices using default tolerance.
-}
matEqual : AffineMatrix -> AffineMatrix -> Expectation
matEqual =
    matEqualTol (AbsoluteOrRelative 1.0e-4 1.0e-4)


{-| Compare two matrices up to a tolerance.
-}
matEqualTol : FloatingPointTolerance -> AffineMatrix -> AffineMatrix -> Expectation
matEqualTol tol expected actual =
    Expect.all
        [ cmpWithin tol (\(AffineMatrix m) -> m.e11) expected
        , cmpWithin tol (\(AffineMatrix m) -> m.e12) expected
        , cmpWithin tol (\(AffineMatrix m) -> m.e21) expected
        , cmpWithin tol (\(AffineMatrix m) -> m.e22) expected
        , cmpWithin tol (\(AffineMatrix m) -> m.tx) expected
        , cmpWithin tol (\(AffineMatrix m) -> m.ty) expected
        ]
        actual
