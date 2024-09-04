module Techdraw.Internal.Svg.Path exposing
    ( toString
    , NFixDigits(..)
    , toFixed
    )

{-| String representation of SVG paths.


# Converting a Path to SVG

@docs toString


# Number Formatting

@docs NFixDigits
@docs toFixed

-}

import List.Nonempty as Nonempty exposing (Nonempty)
import Techdraw.Math as M exposing (OrientationPi, P2)
import Techdraw.Path as P
    exposing
        ( ArcTo
        , CBezierTo
        , Completion
        , LineTo
        , Path
        , QBezierTo
        , Segment
        , Start
        , SubPath
        )


{-| String representation of an SVG path.

This type has a special internal representation of an empty string, to make
joining path strings neater.

-}
type PathString
    = PathString String
    | PathStringEmpty


{-| Convert an `SvgStringPath` to a `String`.
-}
pathStringToString : PathString -> String
pathStringToString ssp =
    case ssp of
        PathString content ->
            content

        PathStringEmpty ->
            ""


{-| Join two string paths together with a space between them. Empty paths are
handled separately to avoid adding unnecessary spaces.
-}
joinWithSpace : PathString -> PathString -> PathString
joinWithSpace sspA sspB =
    case ( sspA, sspB ) of
        ( PathStringEmpty, PathStringEmpty ) ->
            PathStringEmpty

        ( PathString a, PathStringEmpty ) ->
            PathString a

        ( PathStringEmpty, PathString b ) ->
            PathString b

        ( PathString a, PathString b ) ->
            PathString (a ++ " " ++ b)


{-| Join a list of `SvgStringPath` components with spaces.
-}
joinListWithSpace : List PathString -> PathString
joinListWithSpace =
    List.foldl (\r l -> joinWithSpace l r) PathStringEmpty


{-| Join a non-empty list of `SvgStringPath` components with spaces.
-}
joinNonemptyWithSpace : Nonempty PathString -> PathString
joinNonemptyWithSpace =
    Nonempty.foldl (\r l -> joinWithSpace l r) PathStringEmpty


{-| Format a list of items into a single `SvgStringPath`.
-}
formatList : (a -> PathString) -> List a -> PathString
formatList convert =
    List.map convert >> joinListWithSpace


{-| Format a non-empty list of items into a single `SvgStringPath`.
-}
formatNonempty : (a -> PathString) -> Nonempty a -> PathString
formatNonempty convert =
    Nonempty.map convert >> joinNonemptyWithSpace



---- Path Conversion ----------------------------------------------------------


{-| Format a `Path` into a `String` suitable for a `d = "..."` attribute in
SVG.

`NFixDigits` specifies the number of digits to use after the decimal place
in the path string that is produced.

-}
toString : NFixDigits -> Path -> String
toString n (P.Path subPaths) =
    formatList (formatSubPath n) subPaths |> pathStringToString


{-| Format a `SubPath` into an `SvgStringPath`.
-}
formatSubPath : NFixDigits -> SubPath -> PathString
formatSubPath n (P.SubPath completion start segments) =
    joinWithSpace
        (formatStart n start)
        (joinWithSpace
            (formatNonempty (formatSegment n) segments)
            (formatCompletion completion)
        )


{-| Format a `Segment` into an `SvgStringPath`.
-}
formatSegment : NFixDigits -> Segment -> PathString
formatSegment n segment =
    case segment of
        P.SegLineTo lineTo ->
            formatLineTo n lineTo

        P.SegQBezierTo qBezierTo ->
            formatQBezierTo n qBezierTo

        P.SegCBezierTo cBezierTo ->
            formatCBezierTo n cBezierTo

        P.SegArcTo arcTo ->
            formatArcTo n arcTo


{-| Format a `Completion` into an `SvgStringPath` as a "Z" command if the
completion is `Closed`, and an empty `SvgStringPath` if the completion is
`Open`.
-}
formatCompletion : Completion -> PathString
formatCompletion completion =
    case completion of
        P.Open ->
            PathStringEmpty

        P.Closed ->
            PathString "Z"


{-| Format a `Start` into an `SvgStringPath` as a "M" command.
-}
formatStart : NFixDigits -> Start -> PathString
formatStart n (P.Start p) =
    joinWithSpace (PathString "M") (formatP2 n p)


{-| Format a `LineTo` into an `SvgStringPath` as an "L" command.
-}
formatLineTo : NFixDigits -> LineTo -> PathString
formatLineTo n (P.LineTo p) =
    joinWithSpace (PathString "L") (formatP2 n p)


{-| Format a `QBezierTo` into an `SvgStringPath` as a "Q" command.
-}
formatQBezierTo : NFixDigits -> QBezierTo -> PathString
formatQBezierTo n (P.QBezierTo a b) =
    joinListWithSpace
        [ PathString "Q"
        , formatP2 n a
        , formatP2 n b
        ]


{-| Format a `CBezierTo` into an `SvgStringPath` as a "C" command.
-}
formatCBezierTo : NFixDigits -> CBezierTo -> PathString
formatCBezierTo n (P.CBezierTo a b c) =
    joinListWithSpace
        [ PathString "C"
        , formatP2 n a
        , formatP2 n b
        , formatP2 n c
        ]


{-| Format an `ArcTo` into an `SvgStringPath` as an "A" command.

The angular measure in an `ArcTo` is given two additional significant
figures on top of the value in `NFixDigits`.

-}
formatArcTo : NFixDigits -> ArcTo -> PathString
formatArcTo n (P.ArcTo arcTo) =
    joinListWithSpace
        [ PathString "A"
        , formatFloat n arcTo.rx
        , formatFloat n arcTo.ry
        , formatOrientationPi n arcTo.xOrient
        , formatBool arcTo.large
        , formatBool arcTo.sweep
        , formatP2 n arcTo.end
        ]


{-| Format a `P2` into an `SvgStringPath`.
-}
formatP2 : NFixDigits -> P2 -> PathString
formatP2 n p =
    joinWithSpace (formatFloat n (M.p2x p)) (formatFloat n (M.p2y p))


{-| Format a `Float` into an `SvgStringPath`.
-}
formatFloat : NFixDigits -> Float -> PathString
formatFloat n =
    PathString << toFixed n


{-| Format an `OrientationPi` into an `SvgStringPath`.
-}
formatOrientationPi : NFixDigits -> OrientationPi -> PathString
formatOrientationPi n =
    formatFloat n << M.toDegrees << M.getOrientationPi


{-| Format a `Bool` into an `SvgStringPath`.
-}
formatBool : Bool -> PathString
formatBool b =
    PathString <|
        if b then
            "1"

        else
            "0"



---- Number Formatting --------------------------------------------------------


{-| Number of digits to use when formatting a number in fixed precision.
-}
type NFixDigits
    = NFixDigits Int


{-| Format a `Float` as a decimal number with a fixed maximum number of decimal
places.

Both positive and negative numbers are handled correctly:

    toFixed (NFixDigits 3) 3.14159265359
    --> "3.142"

    toFixed (NFixDigits 3) -3.14159265359
    --> "-3.142"

In cases where extra digits are zeroes, and when the decimal place is not
required, they are not produced:

    toFixed (NFixDigits 3) 42
    --> "42"

    toFixed (NFixDigits 3) 42.001
    --> "42.001"

    toFixed (NFixDigits 3) 42.010
    --> "42.01"

    toFixed (NFixDigits 3) 42.100
    --> "42.1"

-}
toFixed : NFixDigits -> Float -> String
toFixed (NFixDigits nDecimals) floatNumber =
    let
        ( sign, x ) =
            if floatNumber < 0 then
                ( "-", -floatNumber )

            else
                ( "", floatNumber )

        scale =
            10 ^ nDecimals

        scaled =
            round (x * toFloat scale)

        whole =
            scaled // scale

        fract =
            modBy scale scaled
    in
    if fract == 0 then
        sign ++ String.fromInt whole

    else
        let
            nLeadingZeros =
                nDecimals - 1 - floor (logBase 10 (toFloat fract))

            leadingZeros =
                String.repeat nLeadingZeros "0"
        in
        sign
            ++ String.fromInt whole
            ++ "."
            ++ leadingZeros
            ++ intStripTrailingZeros fract


{-| Format a positive integer as a string, but without trailing zeroes.
-}
intStripTrailingZeros : Int -> String
intStripTrailingZeros x =
    let
        go : Int -> String
        go xr =
            if xr == 0 then
                ""

            else if modBy 10 xr /= 0 then
                String.fromInt xr

            else
                go (xr // 10)
    in
    go x
