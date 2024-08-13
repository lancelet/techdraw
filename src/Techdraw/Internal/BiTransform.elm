module Techdraw.Internal.BiTransform exposing
    ( BiTransform(..)
    , getFwd, getBwd
    , identity, translation, scaling, rotationRadians
    , applyFwd, applyBwd
    , compose
    )

{-| Bidirectional affine transformations.


# Types

@docs BiTransform


# Functions


## Access

@docs getFwd, getBwd


## Construction

@docs identity, translation, scaling, rotationRadians


## Applying a transformation

@docs applyFwd, applyBwd


## Manipulation

@docs compose

-}

import Techdraw.Internal.AffineMatrix as AffineMatrix exposing (AffineMatrix)


{-| Bidirectional affine transformation.

This stores affine transformation matrices that convert:

  - From the current space to some parent space: `fwd`
  - From some parent space to the current space: `bwd`

-}
type BiTransform
    = BiTransform
        { fwd : AffineMatrix
        , bwd : AffineMatrix
        }


{-| Get the forward transformation.
-}
getFwd : BiTransform -> AffineMatrix
getFwd (BiTransform bt) =
    bt.fwd


{-| Get the backward transformation.
-}
getBwd : BiTransform -> AffineMatrix
getBwd (BiTransform bt) =
    bt.bwd


{-| Identity transformation.
-}
identity : BiTransform
identity =
    BiTransform { fwd = AffineMatrix.identity, bwd = AffineMatrix.identity }


{-| Translation transformation.
-}
translation : ( Float, Float ) -> BiTransform
translation ( tx, ty ) =
    BiTransform
        { fwd = AffineMatrix.translation ( tx, ty )
        , bwd = AffineMatrix.translation ( -tx, -ty )
        }


{-| Scaling transformation.
-}
scaling : ( Float, Float ) -> BiTransform
scaling ( sx, sy ) =
    BiTransform
        { fwd = AffineMatrix.scaling ( sx, sy )
        , bwd = AffineMatrix.scaling ( 1 / sx, 1 / sy )
        }


{-| Rotation transformation (in radians).
-}
rotationRadians : Float -> BiTransform
rotationRadians radAngle =
    BiTransform
        { fwd = AffineMatrix.rotationRadians radAngle
        , bwd = AffineMatrix.rotationRadians -radAngle
        }


{-| Apply a transformation in the forward direction.
-}
applyFwd : BiTransform -> ( Float, Float ) -> ( Float, Float )
applyFwd (BiTransform t) =
    AffineMatrix.mulPt t.fwd


{-| Apply a transformation in the backward direction.
-}
applyBwd : BiTransform -> ( Float, Float ) -> ( Float, Float )
applyBwd (BiTransform t) =
    AffineMatrix.mulPt t.bwd


{-| Compose two transformations.

Ordering is important for transformations. The first transformation in the forward direction
is the first argument, while the second transformation in the forward direction is the
second argument. This is the opposite of the order used for [`AffineMatrix`](#AffineMatrix).

For example, we can start with a point at the origin. We can scale it and then translate it:

    let scale = scaling (5, 4)
        trans = translation (1, 2)
        -- scale then translate
        xform = compose scale trans
    in (applyFwd xform (0, 0), applyBwd xform (1, 2))
    --> ((1, 2), (0, 0))

However, if we reverse the order, the transformations are applied as translation then scane:

    let scale = scaling (5, 4)
        trans = translation (1, 2)
        -- scale then translate
        xform = compose trans scale
    in (applyFwd xform (0, 0), applyBwd xform (5, 8))
    --> ((5, 8), (0, 0))

-}
compose : BiTransform -> BiTransform -> BiTransform
compose (BiTransform first) (BiTransform second) =
    BiTransform
        { fwd = AffineMatrix.mul second.fwd first.fwd
        , bwd = AffineMatrix.mul first.bwd second.bwd
        }
