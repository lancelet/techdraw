module Techdraw.Math.Fuzzer exposing
    ( MinEcc(..)
    , affineTransform
    , affineTransformComponent
    , angle2Pi
    , anglePi
    , m22_affine
    , orientationPi
    , p2
    , path
    , pathNormalized
    , rotation
    , scale
    , shearx
    , subPath
    , subPathNormalized
    , translation
    , v2
    )

{-| Fuzzers (generators) for math types.
-}

import Fuzz exposing (Fuzzer, floatRange)
import List.Nonempty as Nonempty exposing (Nonempty)
import Techdraw.Math as Math
    exposing
        ( AffineTransform(..)
        , AffineTransformComponent
        , Angle2Pi
        , AnglePi
        , ArcTo(..)
        , CBezierTo(..)
        , LineTo(..)
        , M22
        , MoveTo(..)
        , OrientationPi
        , P2
        , Path(..)
        , PathCommand(..)
        , QBezierTo(..)
        , Rotation(..)
        , Scale(..)
        , ShearX(..)
        , SubPath(..)
        , SubPathClosure(..)
        , Translation(..)
        , V2(..)
        )
import Techdraw.Util exposing (unsafeForceMaybe)



---- Fuzzers for angles -------------------------------------------------------


angle2Pi : Fuzzer Angle2Pi
angle2Pi =
    Fuzz.map Math.angle2Pi (floatRange (-4 * pi) (4 * pi))


anglePi : Fuzzer AnglePi
anglePi =
    Fuzz.map Math.anglePi (floatRange (-4 * pi) (4 * pi))


orientationPi : Fuzzer OrientationPi
orientationPi =
    Fuzz.map Math.orientationPi (floatRange (-4 * pi) (4 * pi))



---- Fuzzers for 2D vectors and matrices --------------------------------------


v2 : Fuzzer V2
v2 =
    v2InRange ( -200, 200 ) ( -200, 200 )


v2InRange : ( Float, Float ) -> ( Float, Float ) -> Fuzzer V2
v2InRange ( minE1, maxE1 ) ( minE2, maxE2 ) =
    Fuzz.map2 Math.v2 (floatRange minE1 maxE1) (floatRange minE2 maxE2)


m22_affine : Fuzzer M22
m22_affine =
    Fuzz.map (\(Math.AffineTransform a) -> a.linear) affineTransform



---- Fuzzers for points -------------------------------------------------------


p2 : Fuzzer P2
p2 =
    p2InRange ( -200, 200 ) ( -200, 200 )


p2InRange : ( Float, Float ) -> ( Float, Float ) -> Fuzzer P2
p2InRange xRange yRange =
    Fuzz.map Math.v2p <| v2InRange xRange yRange



---- Fuzzers for AffineTransform Components -----------------------------------


translation : Fuzzer Translation
translation =
    Fuzz.map2
        (\tx ty -> Math.Translation tx ty)
        (floatRange -4 4)
        (floatRange -4 4)


rotation : Fuzzer Rotation
rotation =
    Fuzz.map Math.Rotation angle2Pi


scale : Fuzzer Scale
scale =
    Fuzz.map2 (\sx sy -> Math.Scale sx sy)
        (Fuzz.oneOf [ floatRange -5 -0.2, floatRange 0.2 5 ])
        (Fuzz.oneOf [ floatRange -5 -0.2, floatRange 0.2 5 ])


shearx : Fuzzer ShearX
shearx =
    Fuzz.map (\hx -> Math.ShearX hx)
        (Fuzz.oneOf [ floatRange -3 -0.5, floatRange 0.5 3 ])


affineTransformComponent : Fuzzer AffineTransformComponent
affineTransformComponent =
    Fuzz.oneOf
        [ Fuzz.map Math.ComponentTranslation translation
        , Fuzz.map Math.ComponentRotation rotation
        , Fuzz.map Math.ComponentScale scale
        , Fuzz.map Math.ComponentShearX shearx
        ]



---- Fuzzer for AffineTransform -----------------------------------------------


affineTransform : Fuzzer AffineTransform
affineTransform =
    Fuzz.listOfLengthBetween 1 5 affineTransformComponent
        |> Fuzz.map Math.affFromComponents



---- Fuzzers for Paths --------------------------------------------------------


{-| Minimum eccentricity.

This parameter is used to set a lower bound on the eccentricity of generated
`ArcTo` commands.

To allow circles, set `MinEcc` to a value of zero or lower. 1 indicates
maximum eccentricity.

For many tests, dissimilar radii are preferable. This is because if the radii
are the same, we end up with a circle. In the case of a circle, the principal
axis directions are undefined or degenerate. Consequently, any sophisticated
test that relies on comparing principal axis directions may need them to be
well-defined.

-}
type MinEcc
    = MinEcc Float


path : MinEcc -> Fuzzer Path
path minEcc =
    Fuzz.frequency
        [ ( 1, Fuzz.constant EmptyPath )
        , ( 50, nonEmptyPath minEcc )
        ]


nonEmptyPath : MinEcc -> Fuzzer Path
nonEmptyPath minEcc =
    Fuzz.listOfLengthBetween 1 3 (subPath minEcc)
        |> Fuzz.map Nonempty.fromList
        |> Fuzz.map (unsafeForceMaybe "Generated at least 1 subpath")
        |> Fuzz.map Path


pathNormalized : MinEcc -> Fuzzer Path
pathNormalized minEcc =
    path minEcc |> Fuzz.map Math.normalizePath


subPathNormalized : MinEcc -> Fuzzer SubPath
subPathNormalized minEcc =
    subPath minEcc |> Fuzz.map Math.normalizeSubPath


subPath : MinEcc -> Fuzzer SubPath
subPath minEcc =
    Fuzz.map2
        (\( moveTo, commands ) closure -> SubPath closure moveTo commands)
        (moveToAndPathCommands minEcc)
        subPathClosure


subPathClosure : Fuzzer SubPathClosure
subPathClosure =
    Fuzz.map
        (\flag ->
            if flag then
                SubPathClosed

            else
                SubPathOpen
        )
        Fuzz.bool


moveToAndPathCommands : MinEcc -> Fuzzer ( MoveTo, Nonempty PathCommand )
moveToAndPathCommands minEcc =
    p2
        |> Fuzz.andThen
            (\startPt ->
                pathCommandsOfLengthBetween ( 1, 10 ) minEcc startPt
                    |> Fuzz.map
                        (\pathCommandList ->
                            ( MoveTo startPt
                            , Nonempty.fromList pathCommandList
                                |> unsafeForceMaybe
                                    "Generated at least 1 path command"
                            )
                        )
            )


{-| Generate a list of path commands with a length in a given range and
given a starting point.

The minEccentricity is to control the minimum eccentricity of arc commands.

-}
pathCommandsOfLengthBetween :
    ( Int, Int )
    -> MinEcc
    -> P2
    -> Fuzzer (List PathCommand)
pathCommandsOfLengthBetween ( minLen, maxLen ) minEcc startPt =
    Fuzz.intRange minLen maxLen
        |> Fuzz.andThen
            (\nCommands -> pathCommands nCommands minEcc startPt)


{-| Generate a list of path commands given a starting point.

The minEccentricity is to control the minimum eccentricity of arc commands.

-}
pathCommands : Int -> MinEcc -> P2 -> Fuzzer (List PathCommand)
pathCommands count minEcc startPt =
    let
        addCommand : Int -> P2 -> List PathCommand -> Fuzzer (List PathCommand)
        addCommand cmdCount lastPt cmds =
            if cmdCount == count then
                Fuzz.constant cmds

            else
                pathCommand minEcc lastPt
                    |> Fuzz.andThen
                        (\newCmd ->
                            addCommand
                                (cmdCount + 1)
                                (Math.pathCommandEnd newCmd)
                                (newCmd :: cmds)
                        )
    in
    addCommand 0 startPt [] |> Fuzz.map List.reverse


{-| Generate a path command given the last point on the path.

The minEccentricity is to control the minimum eccentricity of arc commands.

-}
pathCommand : MinEcc -> P2 -> Fuzzer PathCommand
pathCommand minEcc lastPt =
    Fuzz.oneOf
        [ lineTo lastPt |> Fuzz.map CmdLineTo
        , qBezierTo lastPt |> Fuzz.map CmdQBezierTo
        , cBezierTo lastPt |> Fuzz.map CmdCBezierTo
        , arcTo minEcc lastPt |> Fuzz.map CmdArcTo
        ]


{-| Create a next point for a path.
-}
nextPathPoint : P2 -> Fuzzer P2
nextPathPoint lastPt =
    pointMinDistanceFrom ( -200, 200 ) 1 lastPt


lineTo : P2 -> Fuzzer LineTo
lineTo lastPt =
    nextPathPoint lastPt |> Fuzz.map LineTo


qBezierTo : P2 -> Fuzzer QBezierTo
qBezierTo lastPt =
    nextPathPoint lastPt
        |> Fuzz.andThen
            (\a -> nextPathPoint a |> Fuzz.map (\b -> QBezierTo a b))


cBezierTo : P2 -> Fuzzer CBezierTo
cBezierTo lastPt =
    nextPathPoint lastPt
        |> Fuzz.andThen
            (\a ->
                nextPathPoint a
                    |> Fuzz.andThen
                        (\b ->
                            nextPathPoint b
                                |> Fuzz.map (\c -> CBezierTo a b c)
                        )
            )


arcTo : MinEcc -> P2 -> Fuzzer ArcTo
arcTo minEcc lastPt =
    Fuzz.map5
        (\end ( rx, ry ) xOrient large sweep ->
            ArcTo
                { end = end
                , rx = rx
                , ry = ry
                , xOrient = xOrient
                , large = large
                , sweep = sweep
                }
        )
        (nextPathPoint lastPt)
        (arcDissimilarRadii ( 50, 200 ) minEcc)
        orientationPi
        Fuzz.bool
        Fuzz.bool


{-| Generate (potentially) dissimilar radii for an elliptical arc.
-}
arcDissimilarRadii : ( Float, Float ) -> MinEcc -> Fuzzer ( Float, Float )
arcDissimilarRadii ( minRadius, maxRadius ) (MinEcc minEcc) =
    let
        eccentricity rx ry =
            let
                ( a, b ) =
                    ( max rx ry, min rx ry )
            in
            sqrt (1 - Math.sq b / Math.sq a)
    in
    floatRange minRadius maxRadius
        |> Fuzz.andThen
            (\rx ->
                floatRange minRadius maxRadius
                    |> Fuzz.andThen
                        (\ry ->
                            if eccentricity rx ry >= minEcc then
                                Fuzz.constant ( rx, ry )

                            else
                                arcDissimilarRadii
                                    ( minRadius, maxRadius )
                                    (MinEcc minEcc)
                        )
            )


{-| Create a point that is a minimum distance from another point.
-}
pointMinDistanceFrom : ( Float, Float ) -> Float -> P2 -> Fuzzer P2
pointMinDistanceFrom xyRange minDistance referencePt =
    p2InRange xyRange xyRange
        |> Fuzz.andThen
            (\candidateP2 ->
                -- If the candidate point is far enough from the reference
                -- point then keep it. Otherwise, try the whole process
                -- again.
                if Math.p2Distance referencePt candidateP2 >= minDistance then
                    Fuzz.constant candidateP2

                else
                    pointMinDistanceFrom xyRange minDistance referencePt
            )
