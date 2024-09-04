module Techdraw.Math exposing
    ( sq, toRadians, toDegrees
    , Tol(..), closeFloat
    , Angle2Pi, AnglePi, OrientationPi
    , angle2Pi, anglePi, orientationPi
    , getAngle2Pi, getAnglePi, getOrientationPi
    , closeAngle2Pi, closeAnglePi, closeOrientationPi
    , Rotation(..), Scaling(..), ShearingX(..), Translation(..)
    , AffineComponent(..)
    , invertRotation, invertScaling, invertShearingX, invertTranslation
    , invertAffineComponent
    , V2, N2, M22
    , v2, n2, m22
    , v2e1, v2e2
    , n2e1, n2e2
    , m22e11, m22e12, m22e21, m22e22
    , v2n, n2v
    , v2Mag, v2Mul, v2Neg, v2Add, v2Sub, v2Dot, v2ContainsNaN
    , closeV2, closeN2
    , m22MatMul, m22Mul, m22Det, m22Adj, m22Inv, m22Transpose
    , m22v2Mul
    , m22Identity, m22Rotation, m22Scaling, m22ShearingX
    , closeM22
    , P2
    , p2
    , p2x, p2y
    , p2v, v2p
    , p2Add, p2Sub, p2Distance
    , p2ApplyRotation, p2ApplyScaling, p2ApplyShearingX, p2ApplyTranslation
    , p2ApplyAffineComponent
    , p2ApplyAffineTransform
    , closeP2
    , AffineTransform
    , affineTransform, affineTransformMV
    , affIdentity, affRotation, affScaling, affShearingX, affTranslation
    , affFromComponent, affFromComponents
    , affGetLinear, affGetTranslation
    , affMatMul
    , affInvert
    , closeAffineTransform
    )

{-| Mathematical core for Techdraw.

  - [Floating-Point Functions](#floating-point-functions)
  - [Comparison of Floating-Point Values](#comparison-of-floating-point-values)
  - [Angles and Orientations](#angles-and-orientations)
  - [Affine Transformation Components](#affine-transformation-components)
  - [Vectors and Matrices](#vectors-and-matrices)
  - [Points](#points)
  - [Affine Transformations](#affine-transformations)


# Floating Point Functions

@docs sq, toRadians, toDegrees


# Comparison of Floating-Point Values

We have functionality to assist in creating both tests and run-time comparisons
of values.

@docs Tol, closeFloat


# Angles and Orientations

There are two angle types, normalized to particular ranges:

  - [`Angle2Pi`](#Angle2Pi): normalized to `[0, 2*pi)`.
  - [`AnglePi`](#AnglePi): normalized to `(-pi, pi]`.

We have an "orientation":

  - [`OrientationPi`](#OrientationPi): normalized to `[0, pi)`.

An "orientation" here is an angle that specifies a direction, but without a
sign. So, for example, `0` and `pi` represent the same "orientation", because
an infinitely-long line drawn from the origin in both directions, toward
the directions of both `0` and `pi`, is the same line. (This is not standard
mathematical terminology; in many contexts "angle" and "orientation" are used
interchangably. I just couldn't find a name for this concept.)

These angle and orientation types have their constructors hidden so that users
can be sure they are always correctly normalized.

To create these angles use their corresponding smart constructors:

  - [`angle2Pi`](#anglePi)
  - [`anglePi`](#angle2Pi)
  - [`orientationPi`](#orientationPi)

To extract floating-point values from them, use these getters:

  - [`getAngle2Pi`](#getAngle2Pi)
  - [`getAnglePi`](#getAnglePi)
  - [`getOrientationPi`](#getOrientationPi)

For example:

    -- create an Angle2Pi, normalizing the value
    normAngle : AnglePi
    normAngle  = anglePi (10*pi)

    -- fetch the angle value
    angleValue : Float
    angleValue = getAnglePi normAngle

    angleValue
    --> 0.0

@docs Angle2Pi, AnglePi, OrientationPi
@docs angle2Pi, anglePi, orientationPi
@docs getAngle2Pi, getAnglePi, getOrientationPi


## Angle Comparisons

@docs closeAngle2Pi, closeAnglePi, closeOrientationPi


# Affine Transformation Components

We separately represent these individual, invertible affine transformation
components with types:

  - [`Rotation`](#Rotation)
  - [`Scaling`](#Scaling)
  - [`ShearingX`](#ShearingX)
  - [`Translation`](#Translation)

We also collect them into a disjunction called
[`AffineComponent`](#AffineComponent).

These types can only represent one specific type of affine transformation at
a time. The [`AffineTransform`](#AffineTransform) type (see below) can
represent a simultaneous combination of multiple transformations.

See the documentation for
[`Applying Affine Transformation Components`](#applying-affine-transformation-components)
for functions that apply these individual transformation components to points.

@docs Rotation, Scaling, ShearingX, Translation
@docs AffineComponent
@docs invertRotation, invertScaling, invertShearingX, invertTranslation
@docs invertAffineComponent


# Vectors and Matrices

We have the following:

  - [`V2`](#V2): a 2D vector.
  - [`N2`](#N2): a 2D normalized vector.
  - [`M22`](#M22): a 2x2 matrix.


## Types

@docs V2, N2, M22


## Creation

@docs v2, n2, m22


## Element Access

@docs v2e1, v2e2
@docs n2e1, n2e2
@docs m22e11, m22e12, m22e21, m22e22


## Conversion

@docs v2n, n2v


## Vector Operations

@docs v2Mag, v2Mul, v2Neg, v2Add, v2Sub, v2Dot, v2ContainsNaN


## Comparisons

@docs closeV2, closeN2


## Matrix Operations

@docs m22MatMul, m22Mul, m22Det, m22Adj, m22Inv, m22Transpose


## Matrix-Vector Multiply

@docs m22v2Mul


## Construct Transformation Matrix Types

@docs m22Identity, m22Rotation, m22Scaling, m22ShearingX


## Matrix Comparisons

@docs closeM22


# Points

A 2D point is distinguished from a vector type, since it represents a location
in a 2D coordinate system.

  - [`P2`](#P2)

@docs P2


## Creation

@docs p2


## Element Access

@docs p2x, p2y


## Conversion

@docs p2v, v2p


## Operations

@docs p2Add, p2Sub, p2Distance


## Applying Affine Transformation Components

@docs p2ApplyRotation, p2ApplyScaling, p2ApplyShearingX, p2ApplyTranslation
@docs p2ApplyAffineComponent
@docs p2ApplyAffineTransform


## Point Comparisons

@docs closeP2


# Affine Transformations

A type to represent any type of affine transformation. Unlike the
[Affine Transformation Components](#affine-transformation-components), this
type can represent any combined affine transformation. So, for example, it
can represent a simultaneous rotation, translation and scaling.

Affine transformations in this library can be thought of as 3x3 homogeneous
transformation matrices that are multiplied on the left of a homogeneous
column vector representing points. For efficiency reasons, however, only
6 of the 9 components of the transformation matrix are actually stored (since
the remainder are always either `0` or `1`).

See also [`p2ApplyAffineTransform`](#p2ApplyAffineTransform) for the function
that applies an affine transformation to a point.

@docs AffineTransform


## Creation

@docs affineTransform, affineTransformMV
@docs affIdentity, affRotation, affScaling, affShearingX, affTranslation
@docs affFromComponent, affFromComponents


## Extracting Components

@docs affGetLinear, affGetTranslation


## Multiplying Affine Transformations

@docs affMatMul


## Inverting an Affine Transformation

@docs affInvert


## Affine Transform Comparisons

@docs closeAffineTransform

-}

---- Floating-point functions -------------------------------------------------


{-| Square a value.

    sq 4
    --> 16.0

-}
sq : Float -> Float
sq x =
    x * x


{-| Convert an angle in degrees to radians.
-}
toRadians : Float -> Float
toRadians deg =
    deg * pi / 180


{-| Convert an angle in radius to degrees.
-}
toDegrees : Float -> Float
toDegrees rad =
    rad * 180 / pi



---- Comparison of Floating-Point Values --------------------------------------


{-| Tolerance for a floating-point comparison.

The three types of tolerance are:

  - `TolAbs tolAbs`: Absolute tolerance.
  - `TolRel tolRel`: Relative tolerance.
  - `TolAbsRel tolAbs tolRel`: Either absolute or relative tolerance passes.

-}
type Tol
    = TolAbs Float
    | TolRel Float
    | TolAbsRel Float Float


{-| Check if two floating-point values are closer than a given tolerance.

When comparing two `Float` values, `a` and `b`, the following comparisons are
performed:

  - `TolAbs`: `abs (a - b) <= tolAbs`
  - `TolRel`: `abs (a - b) <= tolRel * max (abs a) (abs b)`
  - `TolAbsRel`: either `TolAbs` or `TolRel`

-}
closeFloat : Tol -> Float -> Float -> Bool
closeFloat tol a b =
    case tol of
        TolAbs tolAbs ->
            abs (a - b) <= tolAbs

        TolRel tolRel ->
            abs (a - b) <= tolRel * max (abs a) (abs b)

        TolAbsRel tolAbs tolRel ->
            abs (a - b) <= max tolAbs (tolRel * max (abs a) (abs b))



---- Angles and Orientations --------------------------------------------------


{-| Angle normalized to the range `[0, 2*pi)`.
-}
type Angle2Pi
    = Angle2Pi Float


{-| Angle normalized to the range `(-pi, pi]`.
-}
type AnglePi
    = AnglePi Float


{-| Orientation normalized to the range `[0, pi)`.
-}
type OrientationPi
    = OrientationPi Float


{-| Create an `Angle2Pi`, normalizing to `[0, 2*pi)`.
-}
angle2Pi : Float -> Angle2Pi
angle2Pi =
    norm2pi >> Angle2Pi


{-| Create an `AnglePi`, normalizing to `(-pi, pi]`.
-}
anglePi : Float -> AnglePi
anglePi =
    normPi >> AnglePi


{-| Create an `OrientationPi`, normalizing to `[0, pi)`.
-}
orientationPi : Float -> OrientationPi
orientationPi =
    normOrientationPi >> OrientationPi


{-| Return the value from an `Angle2Pi` as a `Float`.
-}
getAngle2Pi : Angle2Pi -> Float
getAngle2Pi (Angle2Pi angleValue) =
    angleValue


{-| Return the value from an `AnglePi` as a `Float`.
-}
getAnglePi : AnglePi -> Float
getAnglePi (AnglePi angleValue) =
    angleValue


{-| Return the value from an `OrientationPi` as a `Float`.
-}
getOrientationPi : OrientationPi -> Float
getOrientationPi (OrientationPi angleValue) =
    angleValue


{-| The constant `2*pi`.
-}
tau : Float
tau =
    2 * pi


{-| Normalize an angle to `[0, 2*pi)`.
-}
norm2pi : Float -> Float
norm2pi angle =
    if angle >= 0 && angle < 2 * pi then
        angle

    else
        angle - tau * (toFloat <| floor <| angle / tau)


{-| Normalize an angle to `(-pi, pi]`.
-}
normPi : Float -> Float
normPi angle =
    if angle > -pi && angle < pi then
        angle

    else
        let
            pi_angle =
                norm2pi angle
        in
        if pi_angle > pi then
            pi_angle - tau

        else
            pi_angle


{-| Normalize an orientation angle, independent of its sign, to the range
`[0, pi)`.
-}
normOrientationPi : Float -> Float
normOrientationPi angle =
    let
        a1 =
            norm2pi angle
    in
    if a1 < pi then
        a1

    else
        a1 - pi


{-| Check if two `Angle2Pi` values are closer than a given tolerance.

This handles the wrap-around values correctly.

-}
closeAngle2Pi : Tol -> Angle2Pi -> Angle2Pi -> Bool
closeAngle2Pi tol a b =
    closePeriodic tol tau (getAngle2Pi a) (getAngle2Pi b)


{-| Check if two `AnglePi` values are closer than a given tolerance.

This handles the wrap-around values correctly.

-}
closeAnglePi : Tol -> AnglePi -> AnglePi -> Bool
closeAnglePi tol a b =
    closePeriodic tol tau (getAnglePi a) (getAnglePi b)


{-| Check if two `OrientationPi` values are closer than a given tolerance.

This handles the wrap-around values correctly.

-}
closeOrientationPi : Tol -> OrientationPi -> OrientationPi -> Bool
closeOrientationPi tol a b =
    closePeriodic tol pi (getOrientationPi a) (getOrientationPi b)


{-| Check if two values are close, accounting for the wrap-around of values.
-}
closePeriodic : Tol -> Float -> Float -> Float -> Bool
closePeriodic tol period a b =
    -- The values may be in the middle
    closeFloat tol a b
        --  or a may be low and b high
        || closeFloat tol a (b - period)
        -- or b may be low and a high
        || closeFloat tol b (a - period)



---- Affine Transformation Components -----------------------------------------


{-| Rotation by an angle about the origin.
-}
type Rotation
    = Rotation Angle2Pi


{-| Scaling by the given factors along x and y axes.
-}
type Scaling
    = Scaling Float Float


{-| Shear by a factor along the x axis.
-}
type ShearingX
    = ShearingX Float


{-| Translation by a vector.
-}
type Translation
    = Translation V2


{-| Disjunction or sum-type representing any single affine transformation
component.
-}
type AffineComponent
    = AffineRotation Rotation
    | AffineScaling Scaling
    | AffineShearingX ShearingX
    | AffineTranslation Translation


{-| Invert a rotation.
-}
invertRotation : Rotation -> Rotation
invertRotation (Rotation angle) =
    angle |> getAngle2Pi |> negate |> angle2Pi |> Rotation


{-| Invert a scaling.
-}
invertScaling : Scaling -> Scaling
invertScaling (Scaling scaleX scaleY) =
    Scaling (1 / scaleX) (1 / scaleY)


{-| Invert a shearing along x.
-}
invertShearingX : ShearingX -> ShearingX
invertShearingX (ShearingX hx) =
    ShearingX -hx


{-| Invert a translation.
-}
invertTranslation : Translation -> Translation
invertTranslation (Translation translation) =
    translation |> v2Neg |> Translation


{-| Invert any single component of an affine transformation.
-}
invertAffineComponent : AffineComponent -> AffineComponent
invertAffineComponent affineComponent =
    case affineComponent of
        AffineRotation rotation ->
            invertRotation rotation |> AffineRotation

        AffineScaling scaling ->
            invertScaling scaling |> AffineScaling

        AffineShearingX shearingX ->
            invertShearingX shearingX |> AffineShearingX

        AffineTranslation translation ->
            invertTranslation translation |> AffineTranslation



---- Vectors and Matrices -----------------------------------------------------


{-| 2-element vector.
-}
type V2
    = V2 { e1 : Float, e2 : Float }


{-| Create a `V2` vector.
-}
v2 : Float -> Float -> V2
v2 e1 e2 =
    V2 { e1 = e1, e2 = e2 }


{-| Return the first element of a `V2` vector.
-}
v2e1 : V2 -> Float
v2e1 (V2 v) =
    v.e1


{-| Return the second element of a `V2` vector.
-}
v2e2 : V2 -> Float
v2e2 (V2 v) =
    v.e2


{-| Return the length or magnitude of a `V2` vector.
-}
v2Mag : V2 -> Float
v2Mag (V2 v) =
    sqrt (sq v.e1 + sq v.e2)


{-| Multiply a vector by a scalar.
-}
v2Mul : Float -> V2 -> V2
v2Mul c (V2 v) =
    v2 (c * v.e1) (c * v.e2)


{-| Negate a vector.
-}
v2Neg : V2 -> V2
v2Neg =
    v2Mul -1


{-| Add two `V2` vectors.
-}
v2Add : V2 -> V2 -> V2
v2Add (V2 a) (V2 b) =
    v2 (a.e1 + b.e1) (a.e2 + b.e2)


{-| Subtract two `V2` vectors.

    v2Sub (v2 8 7) (v2 2 3)
    --> v2 6 4

-}
v2Sub : V2 -> V2 -> V2
v2Sub (V2 a) (V2 b) =
    v2 (a.e1 - b.e1) (a.e2 - b.e2)


{-| Dot product between vectors.
-}
v2Dot : V2 -> V2 -> Float
v2Dot (V2 a) (V2 b) =
    a.e1 * b.e1 + a.e2 * b.e2


{-| Convert a `V2` vector to a normalized `N2` vector.
-}
v2n : V2 -> N2
v2n v =
    v2Mul (1 / v2Mag v) v |> N2


{-| Returns `True` if either component of a `V2` is a `NaN` value.
-}
v2ContainsNaN : V2 -> Bool
v2ContainsNaN (V2 v) =
    isNaN v.e1 || isNaN v.e2


{-| Check if the components of two `V2` values are close element-wise.
-}
closeV2 : Tol -> V2 -> V2 -> Bool
closeV2 tol a b =
    closeFloat tol (v2e1 a) (v2e1 b) && closeFloat tol (v2e2 a) (v2e2 b)


{-| Check if the components of two `N2` values are close element-wise.
-}
closeN2 : Tol -> N2 -> N2 -> Bool
closeN2 tol a b =
    closeV2 tol (n2v a) (n2v b)


{-| 2-element normalized vector.
-}
type N2
    = N2 V2


{-| Create an `N2` normalized vector.
-}
n2 : Float -> Float -> N2
n2 e1 e2 =
    v2 e1 e2 |> v2n


{-| Convert an `N2` normalized vector to a `V2` regular vector.
-}
n2v : N2 -> V2
n2v (N2 v) =
    v


{-| Return the first element of a `N2` normalized vector.
-}
n2e1 : N2 -> Float
n2e1 =
    n2v >> v2e1


{-| Return the second element of a `N2` normalized vector.
-}
n2e2 : N2 -> Float
n2e2 =
    n2v >> v2e2


{-| 2x2 matrix.
-}
type M22
    = M22
        { e11 : Float
        , e12 : Float
        , e21 : Float
        , e22 : Float
        }


{-| Create an `M22` 2x2 matrix from row-major elements.
-}
m22 : Float -> Float -> Float -> Float -> M22
m22 e11 e12 e21 e22 =
    M22 { e11 = e11, e12 = e12, e21 = e21, e22 = e22 }


{-| Return the element at `(row, col) = (1, 1)` from an `M22` 2x2 matrix.
-}
m22e11 : M22 -> Float
m22e11 (M22 m) =
    m.e11


{-| Return the element at `(row, col) = (1, 2)` from an `M22` 2x2 matrix.
-}
m22e12 : M22 -> Float
m22e12 (M22 m) =
    m.e12


{-| Return the element at `(row, col) = (2, 1)` from an `M22` 2x2 matrix.
-}
m22e21 : M22 -> Float
m22e21 (M22 m) =
    m.e21


{-| Return the element at `(row, col) = (2, 2)` from an `M22` 2x2 matrix.
-}
m22e22 : M22 -> Float
m22e22 (M22 m) =
    m.e22


{-| Perform matrix multiplication between two 2x2 `M22` matrices.
-}
m22MatMul : M22 -> M22 -> M22
m22MatMul (M22 l) (M22 r) =
    m22
        (l.e11 * r.e11 + l.e12 * r.e21)
        (l.e11 * r.e12 + l.e12 * r.e22)
        (l.e21 * r.e11 + l.e22 * r.e21)
        (l.e21 * r.e12 + l.e22 * r.e22)


{-| Multiply a 2x2 `M22` matrix by a scalar.
-}
m22Mul : Float -> M22 -> M22
m22Mul c (M22 m) =
    m22 (c * m.e11) (c * m.e12) (c * m.e21) (c * m.e22)


{-| Return the determinant of a 2x2 `M22` matrix.
-}
m22Det : M22 -> Float
m22Det (M22 m) =
    m.e11 * m.e22 - m.e12 * m.e21


{-| Return the adjugate of a 2x2 `M22` matrix.
-}
m22Adj : M22 -> M22
m22Adj (M22 m) =
    m22 m.e22 -m.e12 -m.e21 m.e11


{-| Return the inverse of a 2x2 `M22` matrix.

No checks are performed to ensure the matrix is invertible.

-}
m22Inv : M22 -> M22
m22Inv m =
    m22Mul (1 / m22Det m) (m22Adj m)


{-| Return the transpose of a 2x2 `M22` matrix.
-}
m22Transpose : M22 -> M22
m22Transpose (M22 m) =
    m22 m.e11 m.e21 m.e12 m.e22


{-| Multiply a 2x2 `M22` matrix by a `V2` column vector.

This performs the multiplication with the `M22` matrix on the left, and the
`V2` vector on the right.

-}
m22v2Mul : M22 -> V2 -> V2
m22v2Mul (M22 m) (V2 v) =
    v2 (m.e11 * v.e1 + m.e12 * v.e2) (m.e21 * v.e1 + m.e22 * v.e2)


{-| The `M22` 2x2 identity matrix.
-}
m22Identity : M22
m22Identity =
    m22 1 0 0 1


{-| Return the 2x2 `M22` matrix that performs the given rotation.
-}
m22Rotation : Rotation -> M22
m22Rotation (Rotation angle) =
    let
        s =
            sin (getAngle2Pi angle)

        c =
            cos (getAngle2Pi angle)
    in
    m22 c -s s c


{-| Return the 2x2 `M22` matrix that performs the given scaling.
-}
m22Scaling : Scaling -> M22
m22Scaling (Scaling scaleX scaleY) =
    m22 scaleX 0 0 scaleY


{-| Return the 2x2 `M22` matrix that performs the given x shearing.
-}
m22ShearingX : ShearingX -> M22
m22ShearingX (ShearingX hx) =
    m22 1 hx 0 1


{-| Check if the elements of two `M22` matrices are close element-wise.
-}
closeM22 : Tol -> M22 -> M22 -> Bool
closeM22 tol a b =
    closeFloat tol (m22e11 a) (m22e11 b)
        && closeFloat tol (m22e12 a) (m22e12 b)
        && closeFloat tol (m22e21 a) (m22e21 b)
        && closeFloat tol (m22e22 a) (m22e22 b)



---- Points -------------------------------------------------------------------


{-| Point in 2D space, in a Cartesian coordinate system.
-}
type P2
    = P2 V2


{-| Create a point from x and y coordinates.
-}
p2 : Float -> Float -> P2
p2 x y =
    P2 <| v2 x y


{-| Return the x component of a point.
-}
p2x : P2 -> Float
p2x =
    p2v >> v2e1


{-| Return the y component of a point.
-}
p2y : P2 -> Float
p2y =
    p2v >> v2e2


{-| Convert a point to a vector.
-}
p2v : P2 -> V2
p2v (P2 v) =
    v


{-| Convert a vector to a point.
-}
v2p : V2 -> P2
v2p =
    P2


{-| Add a vector to a point, producing another point.
-}
p2Add : V2 -> P2 -> P2
p2Add v (P2 p) =
    P2 <| v2Add v p


{-| Subtract a point from another point, producing a vector.

    p2Sub (p2 11 9) (p2 8 4)
    --> v2 3 5

-}
p2Sub : P2 -> P2 -> V2
p2Sub (P2 a) (P2 b) =
    v2Sub a b


{-| Return the distance between two points.
-}
p2Distance : P2 -> P2 -> Float
p2Distance a b =
    p2Sub a b |> v2Mag


{-| Apply a rotation to a point.
-}
p2ApplyRotation : Rotation -> P2 -> P2
p2ApplyRotation (Rotation angle) p =
    let
        c =
            cos (getAngle2Pi angle)

        s =
            sin (getAngle2Pi angle)
    in
    p2 (c * p2x p - s * p2y p) (s * p2x p + c * p2y p)


{-| Apply a scaling to a point.
-}
p2ApplyScaling : Scaling -> P2 -> P2
p2ApplyScaling (Scaling scaleX scaleY) p =
    p2 (scaleX * p2x p) (scaleY * p2y p)


{-| Apply an x shearing to a point.
-}
p2ApplyShearingX : ShearingX -> P2 -> P2
p2ApplyShearingX (ShearingX hx) p =
    p2 (p2x p + hx * p2y p) (p2y p)


{-| Apply a translation to a point.
-}
p2ApplyTranslation : Translation -> P2 -> P2
p2ApplyTranslation (Translation v) p =
    p2Add v p


{-| Apply any single affine transformation component to a point.
-}
p2ApplyAffineComponent : AffineComponent -> P2 -> P2
p2ApplyAffineComponent affineComponent =
    case affineComponent of
        AffineRotation rotation ->
            p2ApplyRotation rotation

        AffineScaling scaling ->
            p2ApplyScaling scaling

        AffineShearingX shearingX ->
            p2ApplyShearingX shearingX

        AffineTranslation translation ->
            p2ApplyTranslation translation


{-| Apply an affine transformation to a point.
-}
p2ApplyAffineTransform : AffineTransform -> P2 -> P2
p2ApplyAffineTransform (AffineTransform m) p =
    p
        |> p2v
        |> m22v2Mul m.linear
        |> v2Add m.translation
        |> v2p


{-| Check if the elements of two `P2` values are close elementwise.
-}
closeP2 : Tol -> P2 -> P2 -> Bool
closeP2 tol a b =
    closeV2 tol (p2v a) (p2v b)



---- Affine Transformations ---------------------------------------------------


{-| 2D
[Affine Transformation (Wikipedia)](https://en.wikipedia.org/wiki/Affine_transformation).
-}
type AffineTransform
    = AffineTransform
        { linear : M22
        , translation : V2
        }


{-| Create an affine transform from its scalar components.

The element order is:

    affineTransform e11 e12 e21 e22 tx ty

-}
affineTransform :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> AffineTransform
affineTransform e11 e12 e21 e22 tx ty =
    AffineTransform { linear = m22 e11 e12 e21 e22, translation = v2 tx ty }


{-| Create an affine transformation from its linear matrix and translation
vector.
-}
affineTransformMV : M22 -> V2 -> AffineTransform
affineTransformMV m v =
    AffineTransform { linear = m, translation = v }


{-| Return the linear component of an affine transform.
-}
affGetLinear : AffineTransform -> M22
affGetLinear (AffineTransform a) =
    a.linear


{-| Return the translation component of an affine transform.
-}
affGetTranslation : AffineTransform -> V2
affGetTranslation (AffineTransform a) =
    a.translation


{-| Zero vector.
-}
v2zero : V2
v2zero =
    v2 0 0


{-| The identity affine transformation.
-}
affIdentity : AffineTransform
affIdentity =
    AffineTransform { linear = m22Identity, translation = v2zero }


{-| Return the affine transformation that performs the given rotation.
-}
affRotation : Rotation -> AffineTransform
affRotation rotation =
    AffineTransform { linear = m22Rotation rotation, translation = v2zero }


{-| Return the affine transformation that performs the given scaling.
-}
affScaling : Scaling -> AffineTransform
affScaling scaling =
    AffineTransform { linear = m22Scaling scaling, translation = v2zero }


{-| Return the affine transformation that performs the given x shearing.
-}
affShearingX : ShearingX -> AffineTransform
affShearingX shearingX =
    AffineTransform { linear = m22ShearingX shearingX, translation = v2zero }


{-| Return the affine transformation that performs the given translation.
-}
affTranslation : Translation -> AffineTransform
affTranslation (Translation translation) =
    AffineTransform { linear = m22Identity, translation = translation }


{-| Return the affine transformation that performs any single affine
transformation.
-}
affFromComponent : AffineComponent -> AffineTransform
affFromComponent affineComponent =
    case affineComponent of
        AffineRotation rotation ->
            affRotation rotation

        AffineScaling scaling ->
            affScaling scaling

        AffineShearingX shearingX ->
            affShearingX shearingX

        AffineTranslation translation ->
            affTranslation translation


{-| Return the affine transformation that performs the list of affine
transformations provided.

The list of components is applied left to right in the list (ie. the first
transformation to be applied is first in the list).

-}
affFromComponents : List AffineComponent -> AffineTransform
affFromComponents =
    List.map affFromComponent >> List.foldl affMatMul affIdentity


{-| Multiply two affine transformations.

Order is important here. Due to the order in which matrices and points are
multiplied in this library, the result of an `affMatMul` is that the affine
transformation on the right is applied first, and the affine transformation
on the left is applied second:

    result =
        affMatMul second first

-}
affMatMul : AffineTransform -> AffineTransform -> AffineTransform
affMatMul (AffineTransform l) (AffineTransform r) =
    AffineTransform
        { linear = m22MatMul l.linear r.linear
        , translation = v2Add (m22v2Mul l.linear r.translation) l.translation
        }


{-| Invert an affine transformation.

No check is performed to ensure that the transformation is invertible.

-}
affInvert : AffineTransform -> AffineTransform
affInvert (AffineTransform m) =
    let
        linear_inv =
            m22Inv m.linear
    in
    AffineTransform
        { linear = linear_inv
        , translation = m22v2Mul linear_inv m.translation |> v2Neg
        }


{-| Check if the components of two `AffineTransform`s are close
element-wise.
-}
closeAffineTransform : Tol -> AffineTransform -> AffineTransform -> Bool
closeAffineTransform tol (AffineTransform a) (AffineTransform b) =
    closeM22 tol a.linear b.linear && closeV2 tol a.translation b.translation
