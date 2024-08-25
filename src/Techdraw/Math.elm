module Techdraw.Math exposing
    ( sq, clamp
    , Angle2Pi, AnglePi, OrientationPi
    , angle2Pi, anglePi, orientationPi
    , getAngle2Pi, getAnglePi, getOrientationPi
    , M22(..), V2(..)
    , m22, v2, v2x, v2y
    , v2Mag, v2Add, v2Sub, v2Norm, v2Scale, v2Div, v2ContainsNaN
    , m22Identity, m22Rotation, m22Scale, m22ShearX
    , m22Mul, m22Div, m22v2Mul, m22Inv, m22Det
    , P2
    , p2, p2x, p2y, p2v, v2p
    , p2Sub, p2Distance
    , AffineTransformComponent(..)
    , Translation(..), Rotation(..), Scale(..), ShearX(..)
    , applyAffineTransformComponent
    , applyTranslation, applyRotation, applyScale, applyShearX
    , invertTranslation, invertRotation, invertScale, invertShearX
    , affFromComponent, affFromComponents
    , AffineTransform(..)
    , affineTransform
    , affIdentity, affTranslate, affScale, affRotate, affShearX
    , affMul, affApplyP2
    , affInvert
    , Line(..), QBezier(..), CBezier(..), Arc(..)
    , Curve(..), JoinedCurves
    , unJoinedCurves
    , MoveTo(..), LineTo(..), QBezierTo(..), CBezierTo(..), ArcTo(..)
    , PathCommand(..)
    , SubPath(..), SubPathClosure(..), Path(..)
    , affApplyPath, affApplySubPath
    , normalizePath, normalizeSubPath
    , subPathClosure
    , pathCommandEnd
    , subPathToJoinedCurves, joinedCurvesToSubPath
    , toSvgPPath
    , ellipseRadiiToBiasedAxes
    , ellipseImplicits
    , ellipseImplicitsToAxes
    )

{-| Mathematical types and operations.


# Floating-Point

@docs sq, clamp


# Angles

It is useful to have angles normalized to particular ranges. The types
provided for this are the following:


## Types

@docs Angle2Pi, AnglePi, OrientationPi


## Functions

@docs angle2Pi, anglePi, orientationPi
@docs getAngle2Pi, getAnglePi, getOrientationPi


# 2D Matrices and Vectors


## Types

2D matrix and vector types:

@docs M22, V2


## Functions

@docs m22, v2, v2x, v2y
@docs v2Mag, v2Add, v2Sub, v2Norm, v2Scale, v2Div, v2ContainsNaN
@docs m22Identity, m22Rotation, m22Scale, m22ShearX
@docs m22Mul, m22Div, m22v2Mul, m22Inv, m22Det


# Points


## Types

@docs P2


## Functions

@docs p2, p2x, p2y, p2v, v2p
@docs p2Sub, p2Distance


# Affine Transformation Components


## Types

@docs AffineTransformComponent
@docs Translation, Rotation, Scale, ShearX


## Functions

@docs applyAffineTransformComponent
@docs applyTranslation, applyRotation, applyScale, applyShearX
@docs invertTranslation, invertRotation, invertScale, invertShearX
@docs affFromComponent, affFromComponents


# Affine transformations


## Types

@docs AffineTransform


## Functions

@docs affineTransform
@docs affIdentity, affTranslate, affScale, affRotate, affShearX
@docs affMul, affApplyP2
@docs affInvert


# Curves


## Types

@docs Line, QBezier, CBezier, Arc
@docs Curve, JoinedCurves


## Functions

@docs unJoinedCurves


# Paths


## Types

@docs MoveTo, LineTo, QBezierTo, CBezierTo, ArcTo
@docs PathCommand
@docs SubPath, SubPathClosure, Path


## Functions

@docs affApplyPath, affApplySubPath
@docs normalizePath, normalizeSubPath
@docs subPathClosure
@docs pathCommandEnd
@docs subPathToJoinedCurves, joinedCurvesToSubPath
@docs toSvgPPath


# Special Operations on Ellipses

@docs ellipseRadiiToBiasedAxes
@docs ellipseImplicits
@docs ellipseImplicitsToAxes

-}

import List.Nonempty as Nonempty exposing (Nonempty)



---- Floating-point functions -------------------------------------------------


{-| Square a value.
-}
sq : Float -> Float
sq x =
    x * x


{-| Clamp a value.
-}
clamp : ( Float, Float ) -> Float -> Float
clamp ( minValue, maxValue ) value =
    if value < minValue then
        minValue

    else if value > maxValue then
        maxValue

    else
        value



---- Angles -------------------------------------------------------------------


{-| Angle normalized to the range `[0, 2*pi)`.

  - Create an `Angle2Pi` using [`angle2Pi`](#angle2Pi).
  - Get the angle as a `Float` using [`getAngle2Pi`](#getAngle2Pi).

-}
type Angle2Pi
    = Angle2Pi Float


{-| Angle normalized to the range `(-pi, pi]`.

  - Create an `AnglePi` using [`anglePi`](#anglePi).
  - Get the angle as a `Float` using [`getAnglePi`](#getAnglePi).

-}
type AnglePi
    = AnglePi Float


{-| Orientation normalized to the range `[0, pi)`.

In our context, an orientation is essentially an angular direction, but
without a sign. In an orientation, we care about the line along which
something is oriented, but not about the positive or negative direction
of that line. Thus, the range of orientations is half the range of a
regular angle. For example, `0` and `pi` are identical orientations,
because the directions they indicate differ only in their sign.

  - Create an `OrientationPi` using [`orientationPi`](#orientationPi).
  - Get the angle as a `Float` using [`getOrientationPi`](#getOrientationPi).

-}
type OrientationPi
    = OrientationPi Float


{-| Create an `Angle2Pi`.

This takes any angle (in radians), normalizes it to the range `[0, 2*pi)`,
and then packages it in an `Angle2Pi`.

-}
angle2Pi : Float -> Angle2Pi
angle2Pi x =
    Angle2Pi <| norm2pi x


{-| Create an `AnglePiPi`.

This takes any angle (in radians), normalizes it to the range `(-pi, pi]`,
and then packages it in an `AnglePiPi`.

-}
anglePi : Float -> AnglePi
anglePi x =
    AnglePi <| normPiPi x


{-| Create an `OrientationPi`.
-}
orientationPi : Float -> OrientationPi
orientationPi x =
    OrientationPi <| normOrientationPi x


{-| Return the angle from an `Angle2Pi`.
-}
getAngle2Pi : Angle2Pi -> Float
getAngle2Pi (Angle2Pi value) =
    value


{-| Return the angle from an `AnglePiPi`.
-}
getAnglePi : AnglePi -> Float
getAnglePi (AnglePi value) =
    value


{-| Return the angle from an `OrientationPi`.
-}
getOrientationPi : OrientationPi -> Float
getOrientationPi (OrientationPi value) =
    value


{-| The constant `2*pi`.
-}
tau : Float
tau =
    2 * pi


{-| Normalize an angle to the range `[0, 2*pi)`.

Zero is inclusive, `2 * pi` is exclusive.

-}
norm2pi : Float -> Float
norm2pi angle =
    if angle >= 0 && angle < 2 * pi then
        angle

    else
        angle - tau * (toFloat <| floor <| angle / tau)


{-| Normalize an angle to the range `(-pi, pi]`.
-}
normPiPi : Float -> Float
normPiPi angle =
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

Sometimes, we care only about the un-signed (non-directed) orientation.
This produces an orientation in a consistent range.

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



---- Matrices and Vectors -----------------------------------------------------


{-| 2x2 matrix.
-}
type M22
    = M22
        { e11 : Float
        , e12 : Float
        , e21 : Float
        , e22 : Float
        }


{-| 2-element vector.
-}
type V2
    = V2
        { e1 : Float
        , e2 : Float
        }


{-| Return the "e1" or "x" component of a vector.
-}
v2x : V2 -> Float
v2x (V2 v) =
    v.e1


{-| Return the "e2" or "y" component of a vector.
-}
v2y : V2 -> Float
v2y (V2 v) =
    v.e2


{-| Create an `M22` matrix from its row-major elements.

    mkM22 e11 e12 e21 e22

-}
m22 : Float -> Float -> Float -> Float -> M22
m22 e11 e12 e21 e22 =
    M22 { e11 = e11, e12 = e12, e21 = e21, e22 = e22 }


{-| Create a `V2` vector from its elements.

    mkV2 e1 e2

-}
v2 : Float -> Float -> V2
v2 e1 e2 =
    V2 { e1 = e1, e2 = e2 }


{-| Length / magnitude of a `V2` vector.
-}
v2Mag : V2 -> Float
v2Mag (V2 v) =
    sqrt (sq v.e1 + sq v.e2)


{-| Divide a vector by its length; producing a normal vector.
-}
v2Norm : V2 -> V2
v2Norm v =
    v2Div (v2Mag v) v


{-| Multiply a vector by a scalar.
-}
v2Scale : Float -> V2 -> V2
v2Scale s (V2 v) =
    v2 (s * v.e1) (s * v.e2)


{-| Divide a vector by a scalar.
-}
v2Div : Float -> V2 -> V2
v2Div d (V2 v) =
    v2 (v.e1 / d) (v.e2 / d)


{-| Add `V2` vectors.
-}
v2Add : V2 -> V2 -> V2
v2Add (V2 l) (V2 r) =
    v2 (l.e1 + r.e1) (l.e2 + r.e2)


{-| Subtract `V2` vectors.
-}
v2Sub : V2 -> V2 -> V2
v2Sub (V2 l) (V2 r) =
    v2 (l.e1 - r.e1) (l.e2 - r.e2)


{-| Check if either component of a `V2` is a NaN value.
-}
v2ContainsNaN : V2 -> Bool
v2ContainsNaN (V2 v) =
    isNaN v.e1 || isNaN v.e2


{-| Create a 2x2 identity matrix.
-}
m22Identity : M22
m22Identity =
    m22 1 0 0 1


{-| Create a 2x2 rotation matrix (rotation about the origin).
-}
m22Rotation : Rotation -> M22
m22Rotation (Rotation angle) =
    let
        a =
            getAngle2Pi angle

        s =
            sin a

        c =
            cos a
    in
    m22 c -s s c


{-| Create a 2x2 scale matrix (scale about the origin).
-}
m22Scale : Scale -> M22
m22Scale (Scale sx sy) =
    m22 sx 0 0 sy


{-| Create a 2x2 shear matrix, shearing along the x axis.
-}
m22ShearX : ShearX -> M22
m22ShearX (ShearX hx) =
    m22 1 hx 0 1


{-| Multiply two `M22` 2x2 matrices.
-}
m22Mul : M22 -> M22 -> M22
m22Mul (M22 l) (M22 r) =
    m22
        (l.e11 * r.e11 + l.e12 * r.e21)
        (l.e11 * r.e12 + l.e12 * r.e22)
        (l.e21 * r.e11 + l.e22 * r.e21)
        (l.e21 * r.e12 + l.e22 * r.e22)


{-| Divide an `M22` matrix by a scalar.
-}
m22Div : Float -> M22 -> M22
m22Div s (M22 m) =
    m22 (m.e11 / s) (m.e12 / s) (m.e21 / s) (m.e22 / s)


{-| Multiply an `M22` 2x2 matrix by a `V2` vector.
-}
m22v2Mul : M22 -> V2 -> V2
m22v2Mul (M22 m) (V2 v) =
    v2 (m.e11 * v.e1 + m.e12 * v.e2) (m.e21 * v.e1 + m.e22 * v.e2)


{-| Invert an `M22` 2x2 matrix.
-}
m22Inv : M22 -> M22
m22Inv (M22 m) =
    let
        d =
            m22Det (M22 m)
    in
    m22 (m.e22 / d) (-m.e12 / d) (-m.e21 / d) (m.e11 / d)


{-| Determinant of an `M22` matrix.
-}
m22Det : M22 -> Float
m22Det (M22 m) =
    m.e11 * m.e22 - m.e12 * m.e21


{-| Transpose an `M22` matrix.
-}
m22Transpose : M22 -> M22
m22Transpose (M22 m) =
    m22 m.e11 m.e21 m.e12 m.e22



---- Points ------------------------------------------------------------------


{-| Point in 2D space.

Internally, this wraps a `V2` vector; indicating that it represents a point.

-}
type P2
    = P2 V2


{-| Return the x-coordinate of a point.
-}
p2x : P2 -> Float
p2x (P2 (V2 v)) =
    v.e1


{-| Return the y-coordinate of a point.
-}
p2y : P2 -> Float
p2y (P2 (V2 v)) =
    v.e2


{-| Construct a point.
-}
p2 : Float -> Float -> P2
p2 x y =
    P2 (v2 x y)


{-| Convert a point to a 2D vector.
-}
p2v : P2 -> V2
p2v (P2 v) =
    v


{-| Convert a 2D vector to a point.
-}
v2p : V2 -> P2
v2p =
    P2


{-| Subtract a point from another point, yielding a vector.
-}
p2Sub : P2 -> P2 -> V2
p2Sub a b =
    v2Sub (p2v a) (p2v b)


{-| Distance between two points.
-}
p2Distance : P2 -> P2 -> Float
p2Distance a b =
    sqrt (sq (p2x a - p2x b) + sq (p2y a - p2y b))



---- Affine Transformation ---------------------------------------------------


{-| [Affine transformation](https://en.wikipedia.org/wiki/Affine_transformation)
for 2D transformations.

The affine transformation is decomposed internally into a linear component
and a translation component, to build on the other math types.

The linear component handles:

  - Rotation.
  - Scaling.
  - Shear / skew.

While the translation component represents the translation only.

-}
type AffineTransform
    = AffineTransform
        { linear : M22
        , translation : V2
        }


{-| Individual component of an affine transformation.
Each component must be invertible in isolation.
-}
type AffineTransformComponent
    = ComponentTranslation Translation
    | ComponentRotation Rotation
    | ComponentScale Scale
    | ComponentShearX ShearX


{-| Apply an individual affine transformation component to a point.
-}
applyAffineTransformComponent : AffineTransformComponent -> P2 -> P2
applyAffineTransformComponent atc v =
    case atc of
        ComponentTranslation t ->
            applyTranslation t v

        ComponentRotation r ->
            applyRotation r v

        ComponentScale s ->
            applyScale s v

        ComponentShearX h ->
            applyShearX h v


{-| Represents a stand-alone translation.
-}
type Translation
    = Translation Float Float


{-| Apply a translation in isolation.
-}
applyTranslation : Translation -> P2 -> P2
applyTranslation (Translation tx ty) p =
    p2 (p2x p + tx) (p2y p + ty)


{-| Invert a translation.
-}
invertTranslation : Translation -> Translation
invertTranslation (Translation tx ty) =
    Translation -tx -ty


{-| Represents a stand-alone rotation.
-}
type Rotation
    = Rotation Angle2Pi


{-| Apply a rotation in isolation.
-}
applyRotation : Rotation -> P2 -> P2
applyRotation (Rotation angle) p =
    let
        c =
            cos (getAngle2Pi angle)

        s =
            sin (getAngle2Pi angle)
    in
    p2 (c * p2x p - s * p2y p) (s * p2x p + c * p2y p)


{-| Invert a rotation.
-}
invertRotation : Rotation -> Rotation
invertRotation (Rotation angle) =
    Rotation <| angle2Pi <| -1 * getAngle2Pi angle


{-| Represents a stand-alone scale.
-}
type Scale
    = Scale Float Float


{-| Apply a scale in isolation.
-}
applyScale : Scale -> P2 -> P2
applyScale (Scale sx sy) p =
    p2 (sx * p2x p) (sy * p2y p)


{-| Invert a scale.
-}
invertScale : Scale -> Scale
invertScale (Scale sx sy) =
    Scale (1 / sx) (1 / sy)


{-| Represents a stand-alone shear in X.
-}
type ShearX
    = ShearX Float


{-| Apply a shear along x in isolation.
-}
applyShearX : ShearX -> P2 -> P2
applyShearX (ShearX hx) p =
    p2 (p2x p + hx * p2y p) (p2y p)


{-| Invert a shear in X.
-}
invertShearX : ShearX -> ShearX
invertShearX (ShearX hx) =
    ShearX -hx


{-| Create a new AffineTransform:

    -- Element order: affineTransform e11 e12 e21 e22 tx ty



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


{-| Create a new identity matrix
-}
affIdentity : AffineTransform
affIdentity =
    AffineTransform { linear = m22Identity, translation = v2 0 0 }


{-| Create an affine transform translating coordinates.
-}
affTranslate : Translation -> AffineTransform
affTranslate (Translation tx ty) =
    AffineTransform { linear = m22Identity, translation = v2 tx ty }


{-| Create an affine transform scaling about the origin.
-}
affScale : Scale -> AffineTransform
affScale scale =
    AffineTransform { linear = m22Scale scale, translation = v2 0 0 }


{-| Create an affine transformation x-shearing about the origin.
-}
affShearX : ShearX -> AffineTransform
affShearX shearx =
    AffineTransform { linear = m22ShearX shearx, translation = v2 0 0 }


{-| Create an affine transform rotating about the origin.
-}
affRotate : Rotation -> AffineTransform
affRotate rotation =
    AffineTransform { linear = m22Rotation rotation, translation = v2 0 0 }


{-| Create an affine transform from any component.
-}
affFromComponent : AffineTransformComponent -> AffineTransform
affFromComponent atc =
    case atc of
        ComponentTranslation t ->
            affTranslate t

        ComponentRotation r ->
            affRotate r

        ComponentScale s ->
            affScale s

        ComponentShearX s ->
            affShearX s


{-| Create an affine transform from a list of components.

The components are applied left to right in the list (ie. the first component
to be applied is first in the list).

-}
affFromComponents : List AffineTransformComponent -> AffineTransform
affFromComponents =
    List.foldl
        (\second first -> affMul (affFromComponent second) first)
        affIdentity


{-| Multiply two affine transformations.

Order is important here. The affine transformation on the right is
applied first, and the affine transformation on the left is applied
second:

    affMul second first

-}
affMul : AffineTransform -> AffineTransform -> AffineTransform
affMul (AffineTransform l) (AffineTransform r) =
    AffineTransform
        { linear = m22Mul l.linear r.linear
        , translation = v2Add (m22v2Mul l.linear r.translation) l.translation
        }


{-| Apply an affine transformation to a point.
-}
affApplyP2 : AffineTransform -> P2 -> P2
affApplyP2 (AffineTransform m) p =
    v2p <| v2Add (m22v2Mul m.linear (p2v p)) m.translation


{-| Invert an affine transformation.
-}
affInvert : AffineTransform -> AffineTransform
affInvert (AffineTransform m) =
    let
        inv_linear =
            m22Inv m.linear
    in
    AffineTransform
        { linear = inv_linear
        , translation = v2Scale -1 (m22v2Mul inv_linear m.translation)
        }



---- Curves -------------------------------------------------------------------


{-| Line
-}
type Line
    = Line P2 P2


{-| Quadratic Bezier curve.
-}
type QBezier
    = QBezier P2 P2 P2


{-| Cubic Bezier curve.
-}
type CBezier
    = CBezier P2 P2 P2 P2


{-| Elliptical arc.
-}
type Arc
    = Arc
        { start : P2
        , end : P2
        , rx : Float
        , ry : Float
        , xOrient : OrientationPi
        , large : Bool
        , sweep : Bool
        }


{-| Available curves.
-}
type Curve
    = CurveLine Line
    | CurveQBezier QBezier
    | CurveCBezier CBezier
    | CurveArc Arc


{-| Non-empty list of joined curves.

The fundametal difference between path commands and curves is:

  - [`PathCommand`](#PathCommand)s only have their end points,
  - [`Curve`](#Curve)s have both start and end points.

A `JoinedCurves` represents the curves contained by a [`SubPath`](#SubPath).
All the curves it contains are joined. Because we have to duplicate the
start and end points between curves, you cannot construct a `JoinedCurves`
value directly. Instead, to construct a `JoinedCurves`, first constrct a
[`SubPath`](#SubPath), and then call
[`subPathToJoinedCurves`](#subPathToJoinedCurves) to convert it.

`JoinedCurves` has the following purposes:

  - Allows `Arc` curves to have their radii normalized.
  - (Future) It allows paths to be shortened.

-}
type JoinedCurves
    = JoinedCurves (Nonempty Curve)


{-| Return the joined curves wrapped by a `JoinedCurves` value.
-}
unJoinedCurves : JoinedCurves -> Nonempty Curve
unJoinedCurves (JoinedCurves cs) =
    cs


{-| Returns the starting point of a curve.
-}
curveStart : Curve -> P2
curveStart curve =
    case curve of
        CurveLine (Line start _) ->
            start

        CurveQBezier (QBezier start _ _) ->
            start

        CurveCBezier (CBezier start _ _ _) ->
            start

        CurveArc (Arc arc) ->
            arc.start


{-| Return the starting point of a `JoinedCurves`.
-}
joinedCurvesStart : JoinedCurves -> P2
joinedCurvesStart cl =
    unJoinedCurves cl |> Nonempty.head |> curveStart


{-| Apply an affine transformation to a `Line`.
-}
affApplyLine : AffineTransform -> Line -> Line
affApplyLine m (Line a b) =
    Line
        (affApplyP2 m a)
        (affApplyP2 m b)


{-| Apply an affine transformation to a `QBezier`.
-}
affApplyQBezier : AffineTransform -> QBezier -> QBezier
affApplyQBezier m (QBezier a b c) =
    QBezier
        (affApplyP2 m a)
        (affApplyP2 m b)
        (affApplyP2 m c)


{-| Apply an affine transformation to a `CBezier`.
-}
affApplyCBezier : AffineTransform -> CBezier -> CBezier
affApplyCBezier m (CBezier a b c d) =
    CBezier
        (affApplyP2 m a)
        (affApplyP2 m b)
        (affApplyP2 m c)
        (affApplyP2 m d)


{-| Apply an affine transformation to an `Arc`.
-}
affApplyArc : AffineTransform -> Arc -> Arc
affApplyArc m (Arc arc) =
    let
        -- Find the ellipse implicits (as though it were at the origin).
        implicits =
            { rx = arc.rx, ry = arc.ry, xOrient = arc.xOrient }
                |> ellipseRadiiToBiasedAxes
                |> ellipseImplicits

        -- Convert the implicits to matrix form.
        implMat =
            m22 implicits.a (implicits.b / 2) (implicits.b / 2) implicits.c

        (AffineTransform minv) =
            affInvert m

        -- Transform the implicit matrix to new coordinates: M^T * (implicits) * M
        (M22 transformedImplMat) =
            m22Mul (m22Transpose minv.linear) (m22Mul implMat minv.linear)

        -- Re-package transformed implicits.
        transformedImplicits =
            { a = transformedImplMat.e11
            , b = transformedImplMat.e12 + transformedImplMat.e21
            , c = transformedImplMat.e22
            }

        -- Transform implicits back to ellipse axis parameters.
        { semiMajor, semiMinor, theta } =
            ellipseImplicitsToAxes transformedImplicits
    in
    Arc
        { start = affApplyP2 m arc.start
        , end = affApplyP2 m arc.end
        , rx = semiMajor
        , ry = semiMinor
        , xOrient = theta
        , large = arc.large
        , sweep = arc.sweep
        }


{-| Apply an affine transformation to a `Curve`.
-}
affApplyCurve : AffineTransform -> Curve -> Curve
affApplyCurve m curve =
    case curve of
        CurveLine line ->
            affApplyLine m line |> CurveLine

        CurveQBezier qBezier ->
            affApplyQBezier m qBezier |> CurveQBezier

        CurveCBezier cBezier ->
            affApplyCBezier m cBezier |> CurveCBezier

        CurveArc arc ->
            affApplyArc m arc |> CurveArc


{-| Apply an affine transformation to a `JoinedCurves`.
-}
affApplyJoinedCurves : AffineTransform -> JoinedCurves -> JoinedCurves
affApplyJoinedCurves m jcs =
    unJoinedCurves jcs |> Nonempty.map (affApplyCurve m) |> JoinedCurves


{-| Result of normalizing an `Arc`.
-}
type ArcNormOutput
    = ArcNormToLine Line
    | ArcNormToArc Arc


{-| Normalize an `Arc`.

This does the following:

1.  If the radii are zero, the `Arc` becomes a line.
2.  Ensure x is the semimajor axis, and y is the semiminor.
3.  Changes negative radii to positive values.
4.  If the radii are too small, replace them with minimal values.

See:
[Correction of out-of-range radii](https://www.w3.org/TR/SVG/implnote.html#ArcCorrectionOutOfRangeRadii)

-}
normalizeArc : Arc -> ArcNormOutput
normalizeArc (Arc a) =
    if a.rx == 0 || a.ry == 0 then
        ArcNormToLine <| Line a.start a.end

    else
        let
            { semiMajor, semiMinor, theta } =
                ellipseRadiiToBiasedAxes
                    { rx = a.rx, ry = a.ry, xOrient = a.xOrient }

            (V2 prime) =
                arcPrime { start = a.start, end = a.end, xOrient = theta }

            l =
                sq prime.e1 / sq semiMajor + sq prime.e2 / sq semiMinor

            ( rx, ry ) =
                if l <= 1 then
                    ( semiMajor, semiMinor )

                else
                    let
                        sqrtl =
                            sqrt l
                    in
                    ( sqrtl * semiMajor, sqrtl * semiMinor )
        in
        Arc
            { start = a.start
            , end = a.end
            , rx = rx
            , ry = ry
            , xOrient = theta
            , large = a.large
            , sweep = a.sweep
            }
            |> ArcNormToArc


{-| Compute some "primed" coordinates on an arc. This is a utility function.

This corresponds to the result for Equation 5.1 here:
<https://www.w3.org/TR/SVG/implnote.html>

-}
arcPrime :
    { start : P2
    , end : P2
    , xOrient : OrientationPi
    }
    -> V2
arcPrime a =
    let
        xOrientF =
            a.xOrient |> getOrientationPi

        c =
            cos xOrientF

        s =
            sin xOrientF

        m =
            m22 c s -s c

        ( x1, y1 ) =
            ( p2x a.start, p2y a.start )

        ( x2, y2 ) =
            ( p2x a.end, p2y a.end )

        v =
            v2Div 2 <| v2 (x1 - x2) (y1 - y2)
    in
    m22v2Mul m v



---- Paths --------------------------------------------------------------------


{-| Move to a point when drawing a path.
-}
type MoveTo
    = MoveTo P2


{-| Draw a straight line to a point when drawing a path.
-}
type LineTo
    = LineTo P2


{-| Draw a quadratic Bezier curve to a point when drawing a path.
-}
type QBezierTo
    = QBezierTo P2 P2


{-| Draw a cubic Bezier curve to a point when drawing a path.
-}
type CBezierTo
    = CBezierTo P2 P2 P2


{-| Draw an elliptical arc to a point when drawing a path.
-}
type ArcTo
    = ArcTo
        { end : P2
        , rx : Float
        , ry : Float
        , xOrient : OrientationPi
        , large : Bool
        , sweep : Bool
        }


{-| Available path drawing commands.
-}
type PathCommand
    = CmdLineTo LineTo
    | CmdQBezierTo QBezierTo
    | CmdCBezierTo CBezierTo
    | CmdArcTo ArcTo


{-| A `SubPath` is a `MoveTo` command followed by a list of drawing commands.

A `SubPath` can be either open or closed.

-}
type SubPath
    = SubPath SubPathClosure MoveTo (Nonempty PathCommand)


{-| Indicates whether a subpath is open or closed.
-}
type SubPathClosure
    = SubPathOpen
    | SubPathClosed


{-| Return the initial `MoveTo` of a `SubPath`.
-}
subPathMoveTo : SubPath -> MoveTo
subPathMoveTo (SubPath _ moveTo _) =
    moveTo


{-| Return the starting point of a `SubPath`.
-}
subPathStart : SubPath -> P2
subPathStart (SubPath _ (MoveTo start) _) =
    start


{-| Returns whether a `SubPath` is open or closed.
-}
subPathClosure : SubPath -> SubPathClosure
subPathClosure (SubPath closure _ _) =
    closure


{-| Return the list of path commands from a `SubPath`.
-}
subPathCommands : SubPath -> Nonempty PathCommand
subPathCommands (SubPath _ _ commands) =
    commands


{-| Get the end point of a `PathCommand`; where we end up after it has been
drawn.
-}
pathCommandEnd : PathCommand -> P2
pathCommandEnd cmd =
    case cmd of
        CmdLineTo (LineTo end) ->
            end

        CmdQBezierTo (QBezierTo _ end) ->
            end

        CmdCBezierTo (CBezierTo _ _ end) ->
            end

        CmdArcTo (ArcTo arc) ->
            arc.end


{-| A `Path` is a list of `SubPath`s.
-}
type Path
    = Path (Nonempty SubPath)
    | EmptyPath


{-| Convert a `SubPath` to a `JoinedCurves`.

This function normalizes any `ArcTo` commands so that they have
valid radii.

-}
subPathToJoinedCurves : SubPath -> JoinedCurves
subPathToJoinedCurves sp =
    let
        go : P2 -> List Curve -> List PathCommand -> List Curve
        go start curves cmds =
            case cmds of
                [] ->
                    curves

                cmd :: rem ->
                    go (pathCommandEnd cmd)
                        (attachStartPathCommandToCurve start cmd :: curves)
                        rem

        -- This exists so we can unwrap a Maybe that we know is
        -- always a Just.
        dummy_curves =
            JoinedCurves <|
                Nonempty.singleton <|
                    CurveLine <|
                        Line (p2 0 0) (p2 100 100)
    in
    go (subPathStart sp) [] (subPathCommands sp |> Nonempty.toList)
        |> List.reverse
        |> Nonempty.fromList
        |> Maybe.map JoinedCurves
        |> Maybe.withDefault dummy_curves


{-| Convert a `PathCommand` to a `Curve` by attaching its starting point.

This process also normalizes `Arc` curves so that they have valid radii.

-}
attachStartPathCommandToCurve : P2 -> PathCommand -> Curve
attachStartPathCommandToCurve start cmd =
    case cmd of
        CmdLineTo (LineTo end) ->
            Line start end |> CurveLine

        CmdQBezierTo (QBezierTo b c) ->
            QBezier start b c |> CurveQBezier

        CmdCBezierTo (CBezierTo b c d) ->
            CBezier start b c d |> CurveCBezier

        CmdArcTo (ArcTo a) ->
            let
                arc_raw =
                    Arc
                        { start = start
                        , end = a.end
                        , rx = a.rx
                        , ry = a.ry
                        , xOrient = a.xOrient
                        , large = a.large
                        , sweep = a.sweep
                        }
            in
            case normalizeArc arc_raw of
                ArcNormToArc arc ->
                    CurveArc arc

                ArcNormToLine line ->
                    CurveLine line


{-| Converts a `JoinedCurves` to a `SubPath`.

This function should be the inverse of
[`subPathToJoinedCurves`](#subPathToJoinedCurves), provided that any arcs in
the original subpath did not require normalization.

-}
joinedCurvesToSubPath : SubPathClosure -> JoinedCurves -> SubPath
joinedCurvesToSubPath closure curves =
    SubPath
        closure
        (MoveTo (joinedCurvesStart curves))
        (Nonempty.map curveToCommand (unJoinedCurves curves))


{-| Convert a `Curve` to a `PathCommand` by stripping its starting point.
-}
curveToCommand : Curve -> PathCommand
curveToCommand curve =
    case curve of
        CurveLine (Line _ end) ->
            LineTo end |> CmdLineTo

        CurveQBezier (QBezier _ b c) ->
            QBezierTo b c |> CmdQBezierTo

        CurveCBezier (CBezier _ b c d) ->
            CBezierTo b c d |> CmdCBezierTo

        CurveArc (Arc arc) ->
            ArcTo
                { end = arc.end
                , rx = arc.rx
                , ry = arc.ry
                , xOrient = arc.xOrient
                , large = arc.large
                , sweep = arc.sweep
                }
                |> CmdArcTo


{-| Apply an affine transformation to a subpath.
-}
affApplySubPath : AffineTransform -> SubPath -> SubPath
affApplySubPath m subpath =
    subPathToJoinedCurves subpath
        |> affApplyJoinedCurves m
        |> joinedCurvesToSubPath (subPathClosure subpath)


{-| Apply an affine transformation to a path.
-}
affApplyPath : AffineTransform -> Path -> Path
affApplyPath m path =
    case path of
        EmptyPath ->
            EmptyPath

        Path pth ->
            pth
                |> Nonempty.map (affApplySubPath m)
                |> Path


{-| Normalize a `SubPath`.

This converts a `SubPath` to [`JoinedCurves`](#JoinedCurves) and back to
normalize it. This only affects the parameterization of
[`ArcTo`](#ArcTo) commands.

-}
normalizeSubPath : SubPath -> SubPath
normalizeSubPath subpath =
    subPathToJoinedCurves subpath
        |> joinedCurvesToSubPath (subPathClosure subpath)


{-| Normalize a `Path`.

This normalizes all sub-paths of a path. This only affects the
parameterization of [`ArcTo`](#ArcTo) commands.

-}
normalizePath : Path -> Path
normalizePath path =
    case path of
        EmptyPath ->
            EmptyPath

        Path cmds ->
            Path <| Nonempty.map normalizeSubPath cmds


{-| Convert a point to SVG path format.
-}
toSvgPPt : P2 -> String
toSvgPPt p =
    String.fromFloat (p2x p) ++ " " ++ String.fromFloat (p2y p)


{-| Convert a `MoveTo` to SVG path format.
-}
toSvgPMoveTo : MoveTo -> String
toSvgPMoveTo (MoveTo p) =
    "M " ++ toSvgPPt p


{-| Convert a `LineTo` to SVG path format.
-}
toSvgPLineTo : LineTo -> String
toSvgPLineTo (LineTo p) =
    "L " ++ toSvgPPt p


{-| Convert a `QBezierTo` to SVG path format.
-}
toSvgPQBezierTo : QBezierTo -> String
toSvgPQBezierTo (QBezierTo a b) =
    "Q " ++ toSvgPPt a ++ " " ++ toSvgPPt b


{-| Convert a `CBezierTo` to SVG path format.
-}
toSvgPCBezierTo : CBezierTo -> String
toSvgPCBezierTo (CBezierTo a b c) =
    "C " ++ toSvgPPt a ++ " " ++ toSvgPPt b ++ " " ++ toSvgPPt c


{-| Convert an `ArcTo` to SVG path format.

`A rx ry x-axis-rotation large-arc-flag sweep-flag x y`

-}
toSvgPArcTo : ArcTo -> String
toSvgPArcTo (ArcTo arc) =
    let
        large =
            if arc.large then
                "1"

            else
                "0"

        sweep =
            if arc.sweep then
                "1"

            else
                "0"
    in
    "A "
        ++ String.fromFloat arc.rx
        ++ " "
        ++ String.fromFloat arc.ry
        ++ " "
        ++ (String.fromFloat <| (\r -> r * 180 / pi) <| getOrientationPi <| arc.xOrient)
        ++ " "
        ++ large
        ++ " "
        ++ sweep
        ++ " "
        ++ toSvgPPt arc.end


{-| Convert a `PathCommand` to SVG path format.
-}
toSvgPPathCommand : PathCommand -> String
toSvgPPathCommand command =
    case command of
        CmdLineTo lineTo ->
            toSvgPLineTo lineTo

        CmdQBezierTo qBezierTo ->
            toSvgPQBezierTo qBezierTo

        CmdCBezierTo cBezierTo ->
            toSvgPCBezierTo cBezierTo

        CmdArcTo arcTo ->
            toSvgPArcTo arcTo


{-| Convert a `SubPath` to SVG path format.
-}
toSvgPSubPath : SubPath -> String
toSvgPSubPath subpath =
    let
        moveto =
            subPathMoveTo subpath |> toSvgPMoveTo

        closing =
            case subPathClosure subpath of
                SubPathClosed ->
                    " Z"

                SubPathOpen ->
                    ""
    in
    subPathCommands subpath
        |> Nonempty.map toSvgPPathCommand
        |> Nonempty.toList
        |> String.join " "
        |> (\cmds -> moveto ++ " " ++ cmds ++ closing)


{-| Convert a `Path` to SVG path format.
-}
toSvgPPath : Path -> String
toSvgPPath path =
    case path of
        EmptyPath ->
            ""

        Path pth ->
            pth
                |> Nonempty.map toSvgPSubPath
                |> Nonempty.toList
                |> String.join " "



---- Ellipse Operations -------------------------------------------------------


{-| Convert ellipse parameters that are expressed without an axis bias to
parameters that have an explicit semi-major and semi-minor axis.

The ellipse parameters are the following:

  - `rx`: radius of the ellipse's local x-axis
  - `ry`: radius of the ellipse's local y-axis
  - `xOrient`: angle between the parent coordinate system's x-axis and the
    ellipse's x-axis

In this form, there is no indication whether x or y is the semi-major axis.
This function converts the parameters into an explicitly-biased
representation, to produce:

  - `semiMajor`: radius of the semi-major axis
  - `semiMinor`: radius of the semi-minor axis
  - `theta`: angle between the parent coordinate system's x-axis and the
    semi-major axis

-}
ellipseRadiiToBiasedAxes :
    { rx : Float
    , ry : Float
    , xOrient : OrientationPi
    }
    ->
        { semiMajor : Float
        , semiMinor : Float
        , theta : OrientationPi
        }
ellipseRadiiToBiasedAxes r =
    let
        ( rx, ry ) =
            ( abs r.rx, abs r.ry )
    in
    if rx > ry then
        { semiMajor = rx, semiMinor = ry, theta = r.xOrient }

    else
        { semiMajor = ry
        , semiMinor = rx
        , theta =
            r.xOrient
                |> getOrientationPi
                |> (\xx -> xx + pi / 2)
                |> orientationPi
        }


{-| Calculate the parameters of the implicit quartic equation of an ellipse
centred at the origin.

The inpus are:

  - `semiMajor`: radius of the semi-major axis
  - `semiMinor`: radius of the semi-minor axis
  - `theta`: angle between the parent coordinate system's x-axis and the
    semi-major axis

The inputs are:

  - `a`: `x^2` coefficient
  - `b`: `x*y` coefficient
  - `c`: `y^2` coefficient

`a`, `b` and `c` form the implicit quartic equation, which is:

    a * sq x + b * x * y + c * sq y - 1 = 0

All points, `(x, y)`, on the ellipse in its parent coordinates must satisfy
this equation.

-}
ellipseImplicits :
    { semiMajor : Float
    , semiMinor : Float
    , theta : OrientationPi
    }
    ->
        { a : Float
        , b : Float
        , c : Float
        }
ellipseImplicits r =
    let
        theta =
            r.theta |> getOrientationPi

        ( c, s ) =
            ( cos theta, sin theta )

        ( c2, s2 ) =
            ( sq c, sq s )

        ( a2, b2 ) =
            ( sq r.semiMajor, sq r.semiMinor )
    in
    { a = c2 / a2 + s2 / b2
    , b = 2 * c * s * (1 / a2 - 1 / b2)
    , c = s2 / a2 + c2 / b2
    }


{-| Convert the parameters of the implicit quadric equation of an ellipse
centred at the origin to the axis form of an ellipse.

See [`ellipseImplicits`](#ellipseImplicits) for a description of all the
parameters.

-}
ellipseImplicitsToAxes :
    { a : Float
    , b : Float
    , c : Float
    }
    ->
        { semiMajor : Float
        , semiMinor : Float
        , theta : OrientationPi
        }
ellipseImplicitsToAxes r =
    let
        c1 =
            sq r.b

        c2 =
            c1 - 4 * r.a * r.c

        c3 =
            r.a + r.c

        c4 =
            sqrt (sq (r.a - r.c) + c1)

        c5 =
            -2 * c2
    in
    { semiMajor = -1 * sqrt (c5 * (c3 + c4)) / c2
    , semiMinor = -1 * sqrt (c5 * (c3 - c4)) / c2
    , theta = atan2 -r.b (r.c - r.a) / 2 |> orientationPi
    }
