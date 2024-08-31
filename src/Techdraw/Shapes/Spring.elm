module Techdraw.Shapes.Spring exposing (spring)

{-| Spring geometry.

@docs spring

-}

import Techdraw.Math as M
import Techdraw.Path as P
import Techdraw.PathBuilder as PB


{-| Path representing a spring.

This is mostly a prototype for now, for more complicated paths.

-}
spring :
    { start : M.P2
    , end : M.P2
    , restLength : Float
    , restCoilHeight : Float
    , restCoilWidth : Float
    , coilCount : Int
    , minEndLength : Float
    , minCoilAngleDeg : Float
    , maxCoilAngleDeg : Float
    }
    -> P.Path
spring r =
    let
        dv =
            M.p2Sub r.end r.start

        xform =
            M.affFromComponents
                [ M.AffineRotation <|
                    M.Rotation <|
                        M.angle2Pi <|
                            atan2 (M.v2e2 dv) (M.v2e1 dv)
                , M.AffineTranslation <|
                    M.Translation (M.p2v r.start)
                ]
    in
    P.pathApplyAffineTransform xform <|
        springAtOriginAlongX
            { curLength = M.v2Mag dv
            , restLength = r.restLength
            , restCoilHeight = r.restCoilHeight
            , restCoilWidth = r.restCoilWidth
            , coilCount = r.coilCount
            , minEndLength = r.minEndLength
            , minCoilAngleDeg = r.minCoilAngleDeg
            , maxCoilAngleDeg = r.maxCoilAngleDeg
            }


springAtOriginAlongX :
    { curLength : Float
    , restLength : Float
    , restCoilHeight : Float
    , restCoilWidth : Float
    , coilCount : Int
    , minEndLength : Float
    , minCoilAngleDeg : Float
    , maxCoilAngleDeg : Float
    }
    -> P.Path
springAtOriginAlongX r =
    let
        restEndLength =
            (r.restLength - r.restCoilWidth) / 2

        minCoilAngleRad =
            r.minCoilAngleDeg * pi / 180

        maxCoilAngleRad =
            r.maxCoilAngleDeg * pi / 180

        singleCoilRestWidth =
            r.restCoilWidth / toFloat r.coilCount

        coilHyp =
            sqrt (M.sq (singleCoilRestWidth / 4) + M.sq (r.restCoilHeight / 2))

        minBasicCoilLength =
            4 * toFloat r.coilCount * coilHyp * cos maxCoilAngleRad

        maxBasicCoilLength =
            4 * toFloat r.coilCount * coilHyp * cos minCoilAngleRad

        -- Compute a basic coil length bounded by the maximum and minimum
        -- basic coil lengths.
        basicCoilLength =
            let
                l =
                    r.curLength - 2 * restEndLength
            in
            if l < minBasicCoilLength then
                minBasicCoilLength

            else if l > maxBasicCoilLength then
                maxBasicCoilLength

            else
                l

        -- Now we know the desired coil length, find the end lengths that
        -- would result from that choice.
        basicEndLength =
            (r.curLength - basicCoilLength) / 2

        -- If the basic end length would be too small, then we have to start
        -- violating the coil angle constraints.
        endLengthA =
            if basicEndLength < r.minEndLength then
                r.minEndLength

            else
                basicEndLength

        -- Finally, if the end length would be too small for the available
        -- space, then we need to voilate the minimum end length too.
        endLengthB =
            if r.curLength < 2 * endLengthA then
                r.curLength / 2

            else
                endLengthA
    in
    resolvedSpringAlongX
        { curLength = r.curLength
        , endLength = endLengthB
        , coilHyp = coilHyp
        , coilCount = r.coilCount
        }


resolvedSpringAlongX :
    { curLength : Float
    , endLength : Float
    , coilHyp : Float
    , coilCount : Int
    }
    -> P.Path
resolvedSpringAlongX r =
    let
        coilWidth =
            r.curLength - 2 * r.endLength

        singleCoilWidth =
            coilWidth / toFloat r.coilCount

        theta =
            acos (singleCoilWidth / 4 / r.coilHyp)

        singleCoilHeight =
            2 * r.coilHyp * sin theta

        springCoils =
            List.map
                (\coilIndex ->
                    let
                        x0 =
                            singleCoilWidth * toFloat coilIndex + r.endLength

                        dx =
                            singleCoilWidth / 4
                    in
                    [ ( x0, 0 )
                    , ( x0 + dx, singleCoilHeight )
                    , ( x0 + 3 * dx, -singleCoilHeight )
                    ]
                )
                (List.range 0 (r.coilCount - 1))
                |> List.concat

        appendSpringCoils pb =
            List.foldl (\xy pbcur -> PB.lineTo xy pbcur) pb springCoils
    in
    PB.empty
        |> PB.moveTo ( 0, 0 )
        |> PB.lineTo ( r.endLength, 0 )
        |> appendSpringCoils
        |> PB.lineTo ( r.curLength - r.endLength, 0 )
        |> PB.lineTo ( r.curLength, 0 )
        |> PB.createPath
