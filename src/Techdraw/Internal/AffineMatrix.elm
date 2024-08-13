module Techdraw.Internal.AffineMatrix exposing
    ( AffineMatrix(..)
    , new, identity, translation, scaling, rotationRadians
    , mul, mulHVec, mulPt, invert
    )

{-| Represents
[Affine transformations](https://en.wikipedia.org/wiki/Affine_transformation) as matrices.


# Types

@docs AffineMatrix


# Functions


## Creation

@docs new, identity, translation, scaling, rotationRadians


## Arithmetic

@docs mul, mulHVec, mulPt, invert

-}


{-| [Affine matrix](https://en.wikipedia.org/wiki/Affine_transformation) for 2D transformations.

The named elements are located in the matrix as follows:

    [ [ e11, e12, tx ]
    , [ e21, e22, ty ]
    , [ 0, 0, 1 ]
    ]

-}
type AffineMatrix
    = AffineMatrix
        { e11 : Float
        , e12 : Float
        , e21 : Float
        , e22 : Float
        , tx : Float
        , ty : Float
        }



---- Creation -------------------------------------------------------------------------------------


{-| Create a new matrix:

    -- Matrix element order: new e11 e12 e21 e22 tx ty
    new 2 0.2 0.1 3 50 20
    --> AffineMatrix { e11=2, e12=0.2, e21=0.1, e22=3, tx=50, ty=20}

See the description of the [`AffineMatrix`](#AffineMatrix) type for the location of these
components in the matrix.

-}
new : Float -> Float -> Float -> Float -> Float -> Float -> AffineMatrix
new e11 e12 e21 e22 tx ty =
    AffineMatrix { e11 = e11, e12 = e12, e21 = e21, e22 = e22, tx = tx, ty = ty }


{-| Create a new identity matrix
-}
identity : AffineMatrix
identity =
    new 1 0 0 1 0 0


{-| Create a translation-only matrix:

    -- translation (tx, ty)
    let mat = translation (5, 4)
        pt  = (2, 8)
    in mulPt mat pt
    --> (7, 12)

-}
translation : ( Float, Float ) -> AffineMatrix
translation ( tx, ty ) =
    new 1 0 0 1 tx ty


{-| Create a scaling-only matrix.

    -- scaling (sx, sy)
    let mat = scaling (2, 3)
        pt  = (2, 4)
    in mulPt mat pt
    --> (4, 12)

-}
scaling : ( Float, Float ) -> AffineMatrix
scaling ( sx, sy ) =
    new sx 0 0 sy 0 0


{-| Create a rotation-only matrix.

    -- rotationRadians angle_in_radians
    let mat    = rotationRadians (pi / 2)
        (x, y) = mulPt mat (100, 0)
    -- round for floating point precision
    in (round x, round y)
    --> (0, 100)

-}
rotationRadians : Float -> AffineMatrix
rotationRadians radians =
    let
        c =
            cos radians

        s =
            sin radians
    in
    new c -s s c 0 0



---- Arithmetic -----------------------------------------------------------------------------------


{-| Matrix multiplication.

Ordering here is important for transformations. The first transformation is the second argument to
this function, and the second transformation is the first argument (ie. transformations are
composed in reverse order).

For example, if we start with a point located at the origin, we can scale it then translate it:

    let scale = scaling (5, 4)
        trans = translation (1, 2)
        -- scale then translate
        m = mul trans scale
    in mulPt m (0, 0)
    --> (1, 2)

However, if we reverse the order of matrix multiplication, we get the transformations applied in
the opposite order:

    let scale = scaling (5, 4)
        trans = translation (1, 2)
        -- translate then scale
        m = mul scale trans
    in mulPt m (0, 0)
    --> (5, 8)

-}
mul : AffineMatrix -> AffineMatrix -> AffineMatrix
mul (AffineMatrix l) (AffineMatrix r) =
    new
        (l.e11 * r.e11 + l.e12 * r.e21)
        (l.e11 * r.e12 + l.e12 * r.e22)
        (l.e21 * r.e11 + l.e22 * r.e21)
        (l.e21 * r.e12 + l.e22 * r.e22)
        (l.e11 * r.tx + l.e12 * r.ty + l.tx)
        (l.e21 * r.tx + l.e22 * r.ty + l.ty)


{-| Multiply a matrix by a homogeneous vector.
-}
mulHVec : AffineMatrix -> ( Float, Float, Float ) -> ( Float, Float, Float )
mulHVec (AffineMatrix m) ( x, y, w ) =
    ( m.e11 * x + m.e12 * y + m.tx * w
    , m.e21 * x + m.e22 * y + m.ty * w
    , w
    )


{-| Multiply a matrix by a point (ie. transform a point).
-}
mulPt : AffineMatrix -> ( Float, Float ) -> ( Float, Float )
mulPt (AffineMatrix m) ( x, y ) =
    ( m.e11 * x + m.e12 * y + m.tx
    , m.e21 * x + m.e22 * y + m.ty
    )


{-| Compute the inverse of the matrix.
-}
invert : AffineMatrix -> AffineMatrix
invert m =
    divScalar (adj m) (det m)


{-| Matrix determinant computation.
-}
det : AffineMatrix -> Float
det (AffineMatrix m) =
    m.e11 * m.e22 - m.e12 * m.e21


{-| Adjoint matrix computation.
-}
adj : AffineMatrix -> AffineMatrix
adj (AffineMatrix m) =
    new
        m.e22
        -m.e12
        -m.e21
        m.e11
        (m.e12 * m.ty - m.tx * m.e22)
        (m.tx * m.e21 - m.e11 * m.ty)


{-| Divide every element in an affine matrix by a scalar.
-}
divScalar : AffineMatrix -> Float -> AffineMatrix
divScalar (AffineMatrix m) c =
    new (m.e11 / c) (m.e12 / c) (m.e21 / c) (m.e22 / c) (m.tx / c) (m.ty / c)
