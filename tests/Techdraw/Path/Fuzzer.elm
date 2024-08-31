module Techdraw.Path.Fuzzer exposing
    ( MinRadius(..), MinEccentricity(..)
    , path, pathNormalizedRadii
    )

{-| Fuzzers for path types.

@docs MinRadius, MinEccentricity
@docs path, pathNormalizedRadii

-}

import Fuzz exposing (Fuzzer)
import List.Nonempty as Nonempty exposing (Nonempty)
import Techdraw.Math as M
import Techdraw.Math.Fuzzer as MF
import Techdraw.Path as P
import Techdraw.Util exposing (unsafeForceMaybe)


{-| Minimum radius for a generated point.
-}
type MinRadius
    = MinRadius Float


{-| Minimum eccentricity for an elliptical arc path segment.
-}
type MinEccentricity
    = MinEccentricity Float


{-| Fuzzer for a `Path` with normalized elliptical arc radii.
-}
pathNormalizedRadii : MinRadius -> MinEccentricity -> Fuzzer P.Path
pathNormalizedRadii minRadius minEccentricity =
    path minRadius minEccentricity |> Fuzz.map P.normalizePathRadii


{-| Fuzzer for `Path`.
-}
path : MinRadius -> MinEccentricity -> Fuzzer P.Path
path minRadius minEccentricity =
    Fuzz.listOfLengthBetween 0 3 (subPath minRadius minEccentricity)
        |> Fuzz.map P.Path


{-| Fuzzer for `SubPath`.
-}
subPath : MinRadius -> MinEccentricity -> Fuzzer P.SubPath
subPath minRadius minEccentricity =
    start
        |> Fuzz.andThen
            (\(P.Start startPt) ->
                Fuzz.map2
                    (\cpln segments ->
                        P.SubPath cpln (P.Start startPt) segments
                    )
                    completion
                    (subPathSegments minRadius startPt minEccentricity)
            )


{-| Fuzzer for `Completion`.
-}
completion : Fuzzer P.Completion
completion =
    Fuzz.bool
        |> Fuzz.map
            (\b ->
                if b then
                    P.Open

                else
                    P.Closed
            )


{-| Fuzzer for `Start`.
-}
start : Fuzzer P.Start
start =
    Fuzz.map P.Start MF.p2


{-| Fuzzer for a variable number of non-empty segments.
-}
subPathSegments :
    MinRadius
    -> M.P2
    -> MinEccentricity
    -> Fuzzer (Nonempty P.Segment)
subPathSegments minRadius p minEccentricity =
    Fuzz.intRange 1 10
        |> Fuzz.andThen
            (\n ->
                segmentNonempty n minRadius p minEccentricity
                    |> Fuzz.map
                        (unsafeForceMaybe "Generated at least 1 segment")
            )


{-| Fuzzer for a non-empty of segments.
-}
segmentNonempty :
    Int
    -> MinRadius
    -> M.P2
    -> MinEccentricity
    -> Fuzzer (Maybe (Nonempty P.Segment))
segmentNonempty n minRadius p minEccentricity =
    Fuzz.map Nonempty.fromList <| segmentList n minRadius p minEccentricity


{-| Fuzzer for a list of segments.
-}
segmentList :
    Int
    -> MinRadius
    -> M.P2
    -> MinEccentricity
    -> Fuzzer (List P.Segment)
segmentList n minRadius p minEccentricity =
    let
        go : Int -> M.P2 -> List P.Segment -> Fuzzer (List P.Segment)
        go nRemaining startPt segs =
            if nRemaining == 0 then
                Fuzz.constant (List.reverse segs)

            else
                segment minRadius startPt minEccentricity
                    |> Fuzz.andThen
                        (\newSeg ->
                            go
                                (nRemaining - 1)
                                (P.endPoint newSeg)
                                (newSeg :: segs)
                        )
    in
    go n p []


{-| Fuzzer for a segment.
-}
segment : MinRadius -> M.P2 -> MinEccentricity -> Fuzzer P.Segment
segment minRadius p minEccentricity =
    Fuzz.oneOf
        [ lineTo minRadius p |> Fuzz.map P.SegLineTo
        , qBezierTo minRadius p |> Fuzz.map P.SegQBezierTo
        , cBezierTo minRadius p |> Fuzz.map P.SegCBezierTo
        , arcTo minRadius p minEccentricity |> Fuzz.map P.SegArcTo
        ]


{-| Fuzzer for `LineTo` with a start point and minimum distance.
-}
lineTo : MinRadius -> M.P2 -> Fuzzer P.LineTo
lineTo minRadius p =
    Fuzz.map P.LineTo (pRad minRadius p)


{-| Fuzzer for `QBezierTo` with a start point and minimum distance.
-}
qBezierTo : MinRadius -> M.P2 -> Fuzzer P.QBezierTo
qBezierTo minRadius p =
    pRad minRadius p
        |> Fuzz.andThen
            (\a ->
                pRad minRadius a
                    |> Fuzz.andThen
                        (\b ->
                            Fuzz.constant <| P.QBezierTo a b
                        )
            )


{-| Fuzzer for `CBezierTo` with a start point and minimum distance.
-}
cBezierTo : MinRadius -> M.P2 -> Fuzzer P.CBezierTo
cBezierTo minRadius p =
    pRad minRadius p
        |> Fuzz.andThen
            (\a ->
                pRad minRadius a
                    |> Fuzz.andThen
                        (\b ->
                            pRad minRadius b
                                |> Fuzz.andThen
                                    (\c ->
                                        Fuzz.constant <| P.CBezierTo a b c
                                    )
                        )
            )


{-| Fuzzer for `ArcTo` with a start point, minimum distance and minimum
eccentricity.
-}
arcTo : MinRadius -> M.P2 -> MinEccentricity -> Fuzzer P.ArcTo
arcTo minRadius p minEccentricity =
    Fuzz.map5
        (\end ( rx, ry ) xOrient large sweep ->
            P.ArcTo
                { end = end
                , rx = rx
                , ry = ry
                , xOrient = xOrient
                , large = large
                , sweep = sweep
                }
        )
        (pRad minRadius p)
        (rxry minEccentricity)
        MF.orientationPi
        Fuzz.bool
        Fuzz.bool



---- Utilities ----------------------------------------------------------------


{-| Fuzz a point which is at least the provided distance away from a given
existing point.
-}
pRad : MinRadius -> M.P2 -> Fuzzer M.P2
pRad (MinRadius minDist) point =
    MF.p2
        |> Fuzz.andThen
            (\genPoint ->
                if M.p2Distance point genPoint >= minDist then
                    Fuzz.constant genPoint

                else
                    pRad (MinRadius minDist) point
            )


{-| Fuzz a pair of radii for an ellipse that match or exceed the provided
minimum eccentricity.
-}
rxry : MinEccentricity -> Fuzzer ( Float, Float )
rxry (MinEccentricity minEccentricity) =
    Fuzz.map2 (\a b -> ( a, b )) erad erad
        |> Fuzz.andThen
            (\( rx, ry ) ->
                let
                    ( a, b ) =
                        ( max (abs rx) (abs ry), min (abs rx) (abs ry) )

                    e =
                        sqrt (1 - M.sq b / M.sq a)
                in
                if e >= minEccentricity then
                    Fuzz.constant ( rx, ry )

                else
                    rxry (MinEccentricity minEccentricity)
            )


{-| Ellipse radius fuzzer.
-}
erad : Fuzzer Float
erad =
    Fuzz.oneOf [ Fuzz.floatRange -50 -1, Fuzz.floatRange 1 50 ]
