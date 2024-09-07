module Depict.Path exposing
    ( Path(..)
    , SubPath(..)
    , Completion(..)
    , Start(..)
    , Segment(..)
    , LineTo(..), QBezierTo(..), CBezierTo(..), ArcTo(..)
    , pathIsEmpty
    , pathApplyAffineTransform
    , normalizePathRadii
    , endPoint
    , closePath
    )

{-| Paths represent lines, curves, and the boundaries of filled shapes which
can also have holes.

Paths are composed of sub-paths. The distinction is that:

  - [`SubPath`](#SubPath): is a connected sequence of segments with no gaps
  - [`Path`](#Path): can have gaps between `SubPath`s.

A sub-path has a start point, but within the sub-path, the end of each
previous segment is always the start of the next segment, so the starting
point of the segments is not recorded. However, within a path, the sub-paths
always start at a specific starting point, which is generally expected to be
different from the end point of a previous sub-path.

Segments inside a sub-path can be any of the following:

  - Lines
  - Quadratic Bezier curves
  - Cubic Bezier curves
  - Elliptical (or circular) arcs


# Types

@docs Path
@docs SubPath
@docs Completion
@docs Start
@docs Segment
@docs LineTo, QBezierTo, CBezierTo, ArcTo


# Operations

@docs pathIsEmpty
@docs pathApplyAffineTransform
@docs normalizePathRadii
@docs endPoint


# Comparisons

@docs closePath

-}

import Depict.Math as M exposing (AffineTransform, OrientationPi, P2, Tol)
import List.Nonempty as Nonempty exposing (Nonempty)


{-| List of sub-paths.

A path consists of a list of sub-paths. A path may contain just one sub-path
(which is a connected sequence of segments), or it may contain multiple
sub-paths to represent things like holes.

An empty `Path` also exists, which is a `Path` that has an empty list of
sub-paths.

-}
type Path
    = Path (List SubPath)


{-| Connected sequence of segments.

A sub-path consists of:

  - [`Completion`](#Completion): indicating whether the sub-path is `Open` or
    `Closed`.
  - [`Start`](#Start): start point of the sub-path.
  - Non-empty list of [`Segment`](#Segment): connected segments of the
    sub-path.

For the segments within a `SubPath`, the end of one segment is always the start
point of the next segment.

-}
type SubPath
    = SubPath Completion Start (Nonempty Segment)


{-| Indicates whether a [`SubPath`](#SubPath) is `Open` or `Closed`.
-}
type Completion
    = Open
    | Closed


{-| Segment that may occur within a [`SubPath`](#SubPath).

When segments appear in a `SubPath`, the end of one segment is always the start
point of the next.

-}
type Segment
    = SegLineTo LineTo
    | SegQBezierTo QBezierTo
    | SegCBezierTo CBezierTo
    | SegArcTo ArcTo


{-| Start point of a [`SubPath`](#SubPath).
-}
type Start
    = Start P2


{-| A segment which draws a line from the current point in a sub-path to the
provided point.
-}
type LineTo
    = LineTo P2


{-| A segment which draws a quadratic Bezier curve.
-}
type QBezierTo
    = QBezierTo P2 P2


{-| A segment which draws a cubic Bezier curve.
-}
type CBezierTo
    = CBezierTo P2 P2 P2


{-| A segment which draws an elliptical arc curve.
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



---- Operations ---------------------------------------------------------------


{-| Check if a path is empty.
-}
pathIsEmpty : Path -> Bool
pathIsEmpty (Path subPaths) =
    List.isEmpty subPaths


{-| Apply an affine transformation to a path.
-}
pathApplyAffineTransform : AffineTransform -> Path -> Path
pathApplyAffineTransform affineTransform (Path subPaths) =
    Path <| List.map (subPathApplyAffineTransform affineTransform) subPaths


subPathApplyAffineTransform : AffineTransform -> SubPath -> SubPath
subPathApplyAffineTransform affineTransform (SubPath completion start segments) =
    SubPath
        completion
        (startApplyAffineTransform affineTransform start)
        (Nonempty.map (segmentApplyAffineTransform affineTransform) segments)


startApplyAffineTransform : AffineTransform -> Start -> Start
startApplyAffineTransform affineTransform (Start p) =
    Start <| M.p2ApplyAffineTransform affineTransform p


segmentApplyAffineTransform : AffineTransform -> Segment -> Segment
segmentApplyAffineTransform transform segment =
    case segment of
        SegLineTo lineTo ->
            SegLineTo <| lineToApplyAffineTransform transform lineTo

        SegQBezierTo qBezierTo ->
            SegQBezierTo <| qBezierToApplyAffineTransform transform qBezierTo

        SegCBezierTo cBezierTo ->
            SegCBezierTo <| cBezierToApplyAffineTransform transform cBezierTo

        SegArcTo arcTo ->
            SegArcTo <| arcToApplyAffineTransform transform arcTo


lineToApplyAffineTransform : AffineTransform -> LineTo -> LineTo
lineToApplyAffineTransform transform (LineTo p) =
    LineTo <| M.p2ApplyAffineTransform transform p


qBezierToApplyAffineTransform : AffineTransform -> QBezierTo -> QBezierTo
qBezierToApplyAffineTransform transform (QBezierTo p1 p2) =
    QBezierTo
        (M.p2ApplyAffineTransform transform p1)
        (M.p2ApplyAffineTransform transform p2)


cBezierToApplyAffineTransform : AffineTransform -> CBezierTo -> CBezierTo
cBezierToApplyAffineTransform transform (CBezierTo p1 p2 p3) =
    CBezierTo
        (M.p2ApplyAffineTransform transform p1)
        (M.p2ApplyAffineTransform transform p2)
        (M.p2ApplyAffineTransform transform p3)


arcToApplyAffineTransform : AffineTransform -> ArcTo -> ArcTo
arcToApplyAffineTransform transform (ArcTo arcTo) =
    let
        -- Find the ellipse implicits.
        implicits =
            { rx = arcTo.rx, ry = arcTo.ry, xOrient = arcTo.xOrient }
                |> ellipseRadiiToBiasedAxes
                |> ellipseImplicits

        -- Convert the implicits to matrix form.
        implMat =
            M.m22 implicits.a (implicits.b / 2) (implicits.b / 2) implicits.c

        -- Inverse of the transformation.
        invTransform =
            M.affInvert transform

        -- Transform the implicit matrix to new coordinates:
        --   M^T * (implicits) * M
        transformedImplMat =
            M.m22MatMul
                (M.m22Transpose (M.affGetLinear invTransform))
                (M.m22MatMul implMat (M.affGetLinear invTransform))

        -- Re-package transformed implicits from matrix form to a record.
        transformedImplicits =
            { a = M.m22e11 transformedImplMat
            , b = M.m22e12 transformedImplMat + M.m22e21 transformedImplMat
            , c = M.m22e22 transformedImplMat
            }

        -- Transform implicits back to ellipse axis parameters.
        { semiMajor, semiMinor, theta } =
            ellipseImplicitsToAxes transformedImplicits
    in
    ArcTo
        { end = M.p2ApplyAffineTransform transform arcTo.end
        , rx = semiMajor
        , ry = semiMinor
        , xOrient = theta
        , large = arcTo.large
        , sweep = arcTo.sweep
        }


{-| Normalize the radii of arc segments in a path.

The radii parameters—`rx`, `ry` and `xOrient`—are normalized so that:

  - `rx` and `ry` are positive, and
  - `rx` is the semi-major axis, `ry` is the semi-minor axis.

This transformation happens automatically if
[`pathApplyAffineTransform`](#pathApplyAffineTransform) is called on a path.

-}
normalizePathRadii : Path -> Path
normalizePathRadii (Path subPaths) =
    Path (List.map normalizeSubPathRadii subPaths)


normalizeSubPathRadii : SubPath -> SubPath
normalizeSubPathRadii (SubPath completion start segments) =
    SubPath completion start (Nonempty.map normalizeSegmentRadii segments)


normalizeSegmentRadii : Segment -> Segment
normalizeSegmentRadii segment =
    case segment of
        SegArcTo arcTo ->
            normalizeArcToRadii arcTo |> SegArcTo

        other ->
            other


normalizeArcToRadii : ArcTo -> ArcTo
normalizeArcToRadii (ArcTo arcTo) =
    { rx = arcTo.rx, ry = arcTo.ry, xOrient = arcTo.xOrient }
        |> ellipseRadiiToBiasedAxes
        |> (\{ semiMajor, semiMinor, theta } ->
                ArcTo
                    { end = arcTo.end
                    , rx = semiMajor
                    , ry = semiMinor
                    , xOrient = theta
                    , large = arcTo.large
                    , sweep = arcTo.sweep
                    }
           )


{-| Return the end-point of a segment.
-}
endPoint : Segment -> P2
endPoint segment =
    case segment of
        SegLineTo (LineTo ep) ->
            ep

        SegQBezierTo (QBezierTo _ ep) ->
            ep

        SegCBezierTo (CBezierTo _ _ ep) ->
            ep

        SegArcTo (ArcTo arcTo) ->
            arcTo.end



---- Comparison Functions -----------------------------------------------------


{-| Check that two paths are equal up to a supplied tolerance.
-}
closePath : Tol -> Path -> Path -> Bool
closePath tol (Path aSubPaths) (Path bSubPaths) =
    List.map2 (closeSubPath tol) aSubPaths bSubPaths |> List.all identity


closeSubPath : Tol -> SubPath -> SubPath -> Bool
closeSubPath tol aSubPath bSubPath =
    let
        (SubPath aCompletion aStart aSegments) =
            aSubPath

        (SubPath bCompletion bStart bSegments) =
            bSubPath
    in
    (aCompletion == bCompletion)
        && closeStart tol aStart bStart
        && (Nonempty.map2 (closeSegment tol) aSegments bSegments
                |> Nonempty.all identity
           )


closeStart : Tol -> Start -> Start -> Bool
closeStart tol (Start aPt) (Start bPt) =
    M.closeP2 tol aPt bPt


closeSegment : Tol -> Segment -> Segment -> Bool
closeSegment tol aSeg bSeg =
    case ( aSeg, bSeg ) of
        ( SegLineTo a, SegLineTo b ) ->
            closeLineTo tol a b

        ( SegQBezierTo a, SegQBezierTo b ) ->
            closeQBezierTo tol a b

        ( SegCBezierTo a, SegCBezierTo b ) ->
            closeCBezierTo tol a b

        ( SegArcTo a, SegArcTo b ) ->
            closeArcTo tol a b

        _ ->
            False


closeLineTo : Tol -> LineTo -> LineTo -> Bool
closeLineTo tol (LineTo a) (LineTo b) =
    M.closeP2 tol a b


closeQBezierTo : Tol -> QBezierTo -> QBezierTo -> Bool
closeQBezierTo tol (QBezierTo a1 a2) (QBezierTo b1 b2) =
    M.closeP2 tol a1 b1 && M.closeP2 tol a2 b2


closeCBezierTo : Tol -> CBezierTo -> CBezierTo -> Bool
closeCBezierTo tol (CBezierTo a1 a2 a3) (CBezierTo b1 b2 b3) =
    M.closeP2 tol a1 b1 && M.closeP2 tol a2 b2 && M.closeP2 tol a3 b3


closeArcTo : Tol -> ArcTo -> ArcTo -> Bool
closeArcTo tol (ArcTo a) (ArcTo b) =
    M.closeP2 tol a.end b.end
        && M.closeFloat tol a.rx b.rx
        && M.closeFloat tol a.ry b.ry
        && M.closeOrientationPi tol a.xOrient b.xOrient
        && (a.large == b.large)
        && (a.sweep == b.sweep)



---- Utility Functions for Elliptical Arc Transformation ----------------------


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
                |> M.getOrientationPi
                |> (\xx -> xx + pi / 2)
                |> M.orientationPi
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
            r.theta |> M.getOrientationPi

        ( c, s ) =
            ( cos theta, sin theta )

        ( c2, s2 ) =
            ( M.sq c, M.sq s )

        ( a2, b2 ) =
            ( M.sq r.semiMajor, M.sq r.semiMinor )
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
            M.sq r.b

        c2 =
            c1 - 4 * r.a * r.c

        c3 =
            r.a + r.c

        c4 =
            sqrt (M.sq (r.a - r.c) + c1)

        c5 =
            -2 * c2
    in
    { semiMajor = -1 * sqrt (c5 * (c3 + c4)) / c2
    , semiMinor = -1 * sqrt (c5 * (c3 - c4)) / c2
    , theta = atan2 -r.b (r.c - r.a) / 2 |> M.orientationPi
    }
