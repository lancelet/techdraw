module Techdraw.Math.Compare exposing
    ( affineTransform
    , affineTransform_tol
    , angle2Pi
    , angle2Pi_tol
    , anglePi
    , anglePi_tol
    , arcTo
    , arcTo_tol
    , cBezierTo
    , cBezierTo_tol
    , lineTo
    , lineTo_tol
    , m22
    , m22_tol
    , moveTo
    , moveTo_tol
    , orientationPi
    , orientationPi_tol
    , p2
    , p2_tol
    , path
    , pathCommand
    , pathCommand_tol
    , path_tol
    , qBezierTo
    , qBezierTo_tol
    , subPath
    , subPath_tol
    , v2
    , v2_tol
    )

{-| Comparison of `Techdraw.Math` types, for testing.
-}

import Expect exposing (FloatingPointTolerance(..))
import Techdraw.Anticipate as Anticipate exposing (Anticipate, any, compareExtracted)
import Techdraw.Math as Math
    exposing
        ( AffineTransform(..)
        , Angle2Pi
        , AnglePi
        , ArcTo(..)
        , CBezierTo(..)
        , LineTo(..)
        , M22(..)
        , MoveTo(..)
        , OrientationPi
        , P2(..)
        , Path(..)
        , PathCommand(..)
        , QBezierTo(..)
        , SubPath(..)
        , V2(..)
        , getAngle2Pi
        , getAnglePi
        , getOrientationPi
        )


defaultTol : FloatingPointTolerance
defaultTol =
    AbsoluteOrRelative 1.0e-6 1.0e-6


angle2Pi : Angle2Pi -> Angle2Pi -> Anticipate
angle2Pi =
    angle2Pi_tol defaultTol


angle2Pi_tol : FloatingPointTolerance -> Angle2Pi -> Angle2Pi -> Anticipate
angle2Pi_tol tol l r =
    Anticipate.within tol (getAngle2Pi l) (getAngle2Pi r)
        |> Anticipate.prependFailMsg "Angle2Pi"


anglePi : AnglePi -> AnglePi -> Anticipate
anglePi =
    anglePi_tol defaultTol


anglePi_tol : FloatingPointTolerance -> AnglePi -> AnglePi -> Anticipate
anglePi_tol tol l r =
    Anticipate.within tol (getAnglePi l) (getAnglePi r)
        |> Anticipate.prependFailMsg "AnglePi"


orientationPi : OrientationPi -> OrientationPi -> Anticipate
orientationPi =
    orientationPi_tol defaultTol


orientationPi_tol :
    FloatingPointTolerance
    -> OrientationPi
    -> OrientationPi
    -> Anticipate
orientationPi_tol tol l r =
    let
        lv =
            getOrientationPi l

        rv =
            getOrientationPi r
    in
    any
        [ -- lv and rv may be in the middle of the range
          Anticipate.within tol lv rv

        -- lv may be low and rv may be high
        , Anticipate.within tol (lv + pi) rv

        -- lv may be high and rv may be low
        , Anticipate.within tol lv (rv + pi)
        ]
        |> Anticipate.prependFailMsg "OrientationPi"


v2 : V2 -> V2 -> Anticipate
v2 =
    v2_tol defaultTol


v2_tol : FloatingPointTolerance -> V2 -> V2 -> Anticipate
v2_tol tol (V2 l) (V2 r) =
    let
        cmp name extract =
            Anticipate.compareExtracted
                name
                extract
                (Anticipate.within tol)
                l
                r
    in
    Anticipate.all
        [ cmp ".e1" .e1
        , cmp ".e2" .e2
        ]
        |> Anticipate.prependFailMsg "V2"


p2 : P2 -> P2 -> Anticipate
p2 l r =
    v2 (Math.p2v l) (Math.p2v r)


p2_tol : FloatingPointTolerance -> P2 -> P2 -> Anticipate
p2_tol tol l r =
    v2_tol tol (Math.p2v l) (Math.p2v r)


m22 : M22 -> M22 -> Anticipate
m22 =
    m22_tol defaultTol


m22_tol : FloatingPointTolerance -> M22 -> M22 -> Anticipate
m22_tol tol (M22 l) (M22 r) =
    let
        cmp name extract =
            Anticipate.compareExtracted
                name
                extract
                (Anticipate.within tol)
                l
                r
    in
    Anticipate.all
        [ cmp ".e11" .e11
        , cmp ".e12" .e12
        , cmp ".e21" .e21
        , cmp ".e22" .e22
        ]
        |> Anticipate.prependFailMsg "M22"


affineTransform : AffineTransform -> AffineTransform -> Anticipate
affineTransform =
    affineTransform_tol (AbsoluteOrRelative 1.0e-3 1.0e-3)


affineTransform_tol :
    FloatingPointTolerance
    -> AffineTransform
    -> AffineTransform
    -> Anticipate
affineTransform_tol tol (AffineTransform l) (AffineTransform r) =
    Anticipate.all
        [ compareExtracted ".linear" .linear (m22_tol tol) l r
        , compareExtracted ".translation" .translation (v2_tol tol) l r
        ]
        |> Anticipate.prependFailMsg "AffineTransform"


moveTo : MoveTo -> MoveTo -> Anticipate
moveTo =
    moveTo_tol defaultTol


moveTo_tol : FloatingPointTolerance -> MoveTo -> MoveTo -> Anticipate
moveTo_tol tol (MoveTo a1) (MoveTo a2) =
    p2_tol tol a1 a2


lineTo : LineTo -> LineTo -> Anticipate
lineTo =
    lineTo_tol defaultTol


lineTo_tol : FloatingPointTolerance -> LineTo -> LineTo -> Anticipate
lineTo_tol tol (LineTo a1) (LineTo a2) =
    p2_tol tol a1 a2


qBezierTo : QBezierTo -> QBezierTo -> Anticipate
qBezierTo =
    qBezierTo_tol defaultTol


qBezierTo_tol : FloatingPointTolerance -> QBezierTo -> QBezierTo -> Anticipate
qBezierTo_tol tol (QBezierTo a1 b1) (QBezierTo a2 b2) =
    Anticipate.all
        [ p2_tol tol a1 a2
        , p2_tol tol b1 b2
        ]


cBezierTo : CBezierTo -> CBezierTo -> Anticipate
cBezierTo =
    cBezierTo_tol defaultTol


cBezierTo_tol : FloatingPointTolerance -> CBezierTo -> CBezierTo -> Anticipate
cBezierTo_tol tol (CBezierTo a1 b1 c1) (CBezierTo a2 b2 c2) =
    Anticipate.all
        [ p2_tol tol a1 a2
        , p2_tol tol b1 b2
        , p2_tol tol c1 c2
        ]


arcTo : ArcTo -> ArcTo -> Anticipate
arcTo =
    arcTo_tol defaultTol


arcTo_tol : FloatingPointTolerance -> ArcTo -> ArcTo -> Anticipate
arcTo_tol tol (ArcTo l) (ArcTo r) =
    Anticipate.all
        [ compareExtracted ".end" .end (p2_tol tol) l r
        , compareExtracted ".rx" .rx (Anticipate.within tol) l r
        , compareExtracted ".ry" .ry (Anticipate.within tol) l r
        , compareExtracted ".xOrient" .xOrient (orientationPi_tol tol) l r
        , compareExtracted ".large" .large Anticipate.equal l r
        , compareExtracted ".sweep" .sweep Anticipate.equal l r
        ]


pathCommand : PathCommand -> PathCommand -> Anticipate
pathCommand =
    pathCommand_tol defaultTol


pathCommand_tol : FloatingPointTolerance -> PathCommand -> PathCommand -> Anticipate
pathCommand_tol tol l r =
    case ( l, r ) of
        ( CmdLineTo x, CmdLineTo y ) ->
            lineTo_tol tol x y

        ( CmdQBezierTo x, CmdQBezierTo y ) ->
            qBezierTo_tol tol x y

        ( CmdCBezierTo x, CmdCBezierTo y ) ->
            cBezierTo_tol tol x y

        ( CmdArcTo x, CmdArcTo y ) ->
            arcTo_tol tol x y

        _ ->
            Anticipate.fail "Different types of path command"


subPath : SubPath -> SubPath -> Anticipate
subPath =
    subPath_tol defaultTol


subPath_tol : FloatingPointTolerance -> SubPath -> SubPath -> Anticipate
subPath_tol tol (SubPath l1 m1 c1) (SubPath l2 m2 c2) =
    Anticipate.all
        [ Anticipate.equal l1 l2
        , moveTo_tol tol m1 m2
        , Anticipate.overNonempty (pathCommand_tol tol) c1 c2
        ]


path : Path -> Path -> Anticipate
path =
    path_tol defaultTol


path_tol : FloatingPointTolerance -> Path -> Path -> Anticipate
path_tol tol pathL pathR =
    case ( pathL, pathR ) of
        ( EmptyPath, EmptyPath ) ->
            Anticipate.pass

        ( Path subPathsL, Path subPathsR ) ->
            Anticipate.overNonempty (subPath_tol tol) subPathsL subPathsR

        ( _, _ ) ->
            Anticipate.fail "Path types were different."
