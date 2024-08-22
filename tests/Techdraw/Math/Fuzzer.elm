module Techdraw.Math.Fuzzer exposing
    ( affineTransform
    , affineTransformComponent
    , angle2Pi
    , anglePi
    , m22_affine
    , orientationPi
    , p2
    , path
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



---- Fuzzers for Nonempty -----------------------------------------------------


nonEmptyOfLengthBetween : Int -> Int -> Fuzzer a -> Fuzzer (Nonempty a)
nonEmptyOfLengthBetween min_val max_val fuzzer =
    let
        real_min =
            max 1 min_val

        real_max =
            max 1 max_val
    in
    Fuzz.listOfLengthBetween real_min real_max fuzzer
        |> Fuzz.map Nonempty.fromList
        |> Fuzz.map (unsafeForceMaybe "Generated more than one element")



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
    Fuzz.map2
        (\e1 e2 -> Math.v2 e1 e2)
        (floatRange -2000 2000)
        (floatRange -2000 200)


m22_affine : Fuzzer M22
m22_affine =
    Fuzz.map (\(Math.AffineTransform a) -> a.linear) affineTransform



---- Fuzzers for points -------------------------------------------------------


p2 : Fuzzer P2
p2 =
    Fuzz.map Math.v2p v2



---- Fuzzers for AffineTransform Components -----------------------------------


translation : Fuzzer Translation
translation =
    Fuzz.map2
        (\tx ty -> Math.Translation tx ty)
        (floatRange -2000 2000)
        (floatRange -2000 2000)


rotation : Fuzzer Rotation
rotation =
    Fuzz.map Math.Rotation angle2Pi


scale : Fuzzer Scale
scale =
    Fuzz.map2 (\sx sy -> Math.Scale sx sy)
        (Fuzz.oneOf [ floatRange -10 -0.05, floatRange 0.05 10 ])
        (Fuzz.oneOf [ floatRange -10 -0.05, floatRange 0.05 10 ])


shearx : Fuzzer ShearX
shearx =
    Fuzz.map (\hx -> Math.ShearX hx)
        (Fuzz.oneOf [ floatRange -10 -0.05, floatRange 0.05 10 ])


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


path : Fuzzer Path
path =
    Fuzz.map Path (nonEmptyOfLengthBetween 1 3 subPath)


subPath : Fuzzer SubPath
subPath =
    Fuzz.map3 SubPath subPathClosure moveTo (nonEmptyOfLengthBetween 1 32 pathCommand)


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


{-| Fuzzer for sub-paths with normalized arcs.
-}
subPathNormalized : Fuzzer SubPath
subPathNormalized =
    Fuzz.map Math.normalizeSubPath subPath


pathCommand : Fuzzer PathCommand
pathCommand =
    Fuzz.oneOf
        [ Fuzz.map CmdLineTo lineTo
        , Fuzz.map CmdQBezierTo qBezierTo
        , Fuzz.map CmdCBezierTo cBezierTo
        , Fuzz.map CmdArcTo arcTo
        ]


moveTo : Fuzzer MoveTo
moveTo =
    Fuzz.map Math.MoveTo p2


lineTo : Fuzzer LineTo
lineTo =
    Fuzz.map Math.LineTo p2


qBezierTo : Fuzzer QBezierTo
qBezierTo =
    Fuzz.map2 Math.QBezierTo p2 p2


cBezierTo : Fuzzer CBezierTo
cBezierTo =
    Fuzz.map3 Math.CBezierTo p2 p2 p2


arcTo : Fuzzer ArcTo
arcTo =
    Fuzz.map6
        (\end rx ry xOrient large sweep ->
            ArcTo
                { end = end
                , rx = rx
                , ry = ry
                , xOrient = xOrient
                , large = large
                , sweep = sweep
                }
        )
        p2
        (floatRange -2000 2000)
        (floatRange -2000 2000)
        orientationPi
        Fuzz.bool
        Fuzz.bool
