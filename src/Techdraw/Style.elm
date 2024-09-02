module Techdraw.Style exposing
    ( Style(..)
    , Option(..)
    , Fill(..)
    , FillRule(..)
    , Stroke(..)
    , LineCap(..), LineJoin(..), DashArray(..)
    , Paint(..)
    , LinearGradientParams, LinearGradient
    , RadialGradientParams, RadialGradient
    , linearGradient, radialGradient
    , Gradient, Stop(..), gradient
    , gradientHexHash
    , linearGradientHexHash, linearGradientParams
    , radialGradientHexHash, radialGradientParams
    , combineStyle
    , inheritAll
    , fill, fillRule
    , stroke, strokeWidth, lineCap, lineJoin, dashArray, dashOffset
    )

{-| Styles.


# Types


## Overall Style

@docs Style


## Style Options

@docs Option


## Fill

@docs Fill
@docs FillRule


## Stroke

@docs Stroke
@docs LineCap, LineJoin, DashArray


## Paint

@docs Paint


## Gradients

@docs LinearGradientParams, LinearGradient
@docs RadialGradientParams, RadialGradient
@docs linearGradient, radialGradient
@docs Gradient, Stop, gradient
@docs gradientHexHash
@docs linearGradientHexHash, linearGradientParams
@docs radialGradientHexHash, radialGradientParams


# Operations

@docs combineStyle


# Style Setting Functions

@docs inheritAll
@docs fill, fillRule
@docs stroke, strokeWidth, lineCap, lineJoin, dashArray, dashOffset

-}

import Bytes.Encode as Encode exposing (Encoder)
import Color exposing (Color)
import SHA1
import Techdraw.Internal.Codec as Codec
import Techdraw.Math as Math exposing (AffineTransform, P2)



---- Overall Style ------------------------------------------------------------


{-| Style.
-}
type Style
    = Style
        { fill : Fill
        , stroke : Stroke
        }



---- Style Option -------------------------------------------------------------


{-| Style option.

A style option can be inherited or set.

-}
type Option a
    = Inherit
    | Set a



---- Fills --------------------------------------------------------------------


{-| Fill style.
-}
type Fill
    = Fill
        { fill : Option Paint
        , fillRule : Option FillRule
        }


{-| Fill rule.
-}
type FillRule
    = NonZero
    | EvenOdd



---- Strokes ------------------------------------------------------------------


{-| Stroke style.
-}
type Stroke
    = Stroke
        { stroke : Option Paint
        , strokeWidth : Option Float
        , lineCap : Option LineCap
        , lineJoin : Option LineJoin
        , dashArray : Option DashArray
        , dashOffset : Option Float
        }


{-| Line cap.
-}
type LineCap
    = LineCapButt
    | LineCapRound
    | LineCapSquare


{-| Line join.
-}
type LineJoin
    = LineJoinMiter Float
    | LineJoinMiterClip Float
    | LineJoinRound
    | LineJoinBevel
    | LineJoinArcs


{-| Line dash array.
-}
type DashArray
    = DashArray (List Float)



---- Paint --------------------------------------------------------------------


{-| Paint lines or fills.
-}
type Paint
    = Paint Color
    | PaintLinearGradient LinearGradient
    | PaintRadialGradient RadialGradient



---- Gradients ----------------------------------------------------------------


{-| Parameters of a linear gradient.

A linear gradient is specified by:

  - `start`: a start point.
  - `end`: an end point.
  - `transform`: additional gradient transform, which maps from the gradient
    coordinate system to the local coordinate system.
  - `gradient`: the list of gradient stops.

-}
type alias LinearGradientParams =
    { start : P2
    , end : P2
    , transform : AffineTransform
    , gradient : Gradient
    }


{-| Linear gradient.

Internally, this contains the parameters of the linear gradient along with
a unique hash.

-}
type LinearGradient
    = LinearGradient SHA1.Digest UnhashedLinearGradient


{-| Unhashed linear gradient.
-}
type UnhashedLinearGradient
    = UnhashedLinearGradient LinearGradientParams


{-| Create a linear gradient.
-}
linearGradient : LinearGradientParams -> LinearGradient
linearGradient record =
    let
        ulg =
            UnhashedLinearGradient record
    in
    LinearGradient (hashUnhashedLinearGradient ulg) ulg


{-| Parameters of a radial gradient.

A radial gradient is drawn between an "inner circle" and an "outer circle".
It is specified by:

  - `innerCenter`: center of the inner circle.
  - `innerRadius`: radius of the inner circle.
  - `outerCenter`: center of the outer circle.
  - `outerRadius`: radius of the outer circle.
  - `transform`: additional gradient transform, which maps from the gradient
    coordinate system to the local coordinate system.
  - `gradient`: the list of gradient stops.

-}
type alias RadialGradientParams =
    { innerCenter : P2
    , innerRadius : Float
    , outerCenter : P2
    , outerRadius : Float
    , transform : AffineTransform
    , gradient : Gradient
    }


{-| Radial gradient.

Internally, this contains the parameters of the radial gradient along with a
unique hash.

-}
type RadialGradient
    = RadialGradient SHA1.Digest UnhashedRadialGradient


{-| Unhashed radial gradient.
-}
type UnhashedRadialGradient
    = UnhashedRadialGradient RadialGradientParams


{-| Create a radial gradient.
-}
radialGradient : RadialGradientParams -> RadialGradient
radialGradient record =
    let
        urg =
            UnhashedRadialGradient record
    in
    RadialGradient (hashUnhashedRadialGradient urg) urg


{-| Gradient.

A gradient contains

  - A list of stops.
  - Hashing information for the stops, so that a global unique ID can be created
    for the gradient.

-}
type Gradient
    = Gradient SHA1.Digest UnhashedGradient


{-| Gradient without any hashing information.
-}
type UnhashedGradient
    = UnhashedGradient (List Stop)


{-| Gradient stop.

This specifies a location (usually a number between 0 and 1) for the stop,
and a color that specifies the color of the gradient stop.

-}
type Stop
    = Stop Float Color


{-| Return the location of a gradient stop.
-}
getStopLocation : Stop -> Float
getStopLocation (Stop location _) =
    location


{-| Create a `Gradient` from a list of stops.
-}
gradient : List Stop -> Gradient
gradient rawStops =
    let
        unhashed =
            UnhashedGradient <| List.sortBy getStopLocation rawStops
    in
    Gradient (hashUnhashedGradient unhashed) unhashed


{-| Return a hex string containing the hash of a `Gradient`.

    import Color

    gradientHexHash <|
        gradient
            [ Stop 0.0 Color.black
            , Stop 0.5 Color.blue
            , Stop 0.7 Color.red
            , Stop 1.0 Color.green
            ]
    --> "4233f96f9909e03055aeb16861262ec25214bcc4"

The hash should be the same if `Stop`s are re-ordered; eg:

    import Color

    gradientHexHash <|
        gradient
            [ Stop 1.0 Color.green
            , Stop 0.0 Color.black
            , Stop 0.7 Color.red
            , Stop 0.5 Color.blue
            ]
    --> "4233f96f9909e03055aeb16861262ec25214bcc4"

But will be different if the `Stop`s are different:

    import Color

    gradientHexHash <|
        gradient
            [ Stop 0.0 Color.black
            , Stop 1.0 Color.white
            ]
    --> "59bc04f8c6298d5bcbe6f89ed20ec316cc8a5959"

-}
gradientHexHash : Gradient -> String
gradientHexHash (Gradient digest _) =
    SHA1.toHex digest


{-| Return the hex hash of a linear gradient.
-}
linearGradientHexHash : LinearGradient -> String
linearGradientHexHash (LinearGradient digest _) =
    SHA1.toHex digest


{-| Return the parameters of a linear gradient.
-}
linearGradientParams : LinearGradient -> LinearGradientParams
linearGradientParams (LinearGradient _ (UnhashedLinearGradient params)) =
    params


{-| Return the hex hash of a radial gradient.
-}
radialGradientHexHash : RadialGradient -> String
radialGradientHexHash (RadialGradient digest _) =
    SHA1.toHex digest


{-| Return the parameters of a radial gradient.
-}
radialGradientParams : RadialGradient -> RadialGradientParams
radialGradientParams (RadialGradient _ (UnhashedRadialGradient params)) =
    params


{-| Return the SHA1 Digest of a `Gradient`.
-}
gradientSHA1Digest : Gradient -> SHA1.Digest
gradientSHA1Digest (Gradient digest _) =
    digest



---- Combining Styles ---------------------------------------------------------


{-| Combine style options from a parent and child.

    outcomeStyle =
        combineStyle parentStyle childStyle

-}
combineStyle : Style -> Style -> Style
combineStyle (Style parent) (Style child) =
    Style
        { fill = combineFill parent.fill child.fill
        , stroke = combineStroke parent.stroke child.stroke
        }


{-| Combine fill styling options from a parent and child.
-}
combineFill : Fill -> Fill -> Fill
combineFill (Fill parent) (Fill child) =
    let
        cmb extract =
            combineExtracted extract parent child
    in
    Fill
        { fill = cmb .fill
        , fillRule = cmb .fillRule
        }


{-| Combine stroke styling options from a parent and child.
-}
combineStroke : Stroke -> Stroke -> Stroke
combineStroke (Stroke parent) (Stroke child) =
    let
        cmb extract =
            combineExtracted extract parent child
    in
    Stroke
        { stroke = cmb .stroke
        , strokeWidth = cmb .strokeWidth
        , lineCap = cmb .lineCap
        , lineJoin = cmb .lineJoin
        , dashArray = cmb .dashArray
        , dashOffset = cmb .dashOffset
        }


{-| Combine a styling option extracted from a type.
-}
combineExtracted : (a -> Option b) -> a -> a -> Option b
combineExtracted extract parent child =
    combineOption (extract parent) (extract child)


{-| Combine a parent and child styling option.

1.  The child `Set` option always takes precedence.
2.  If the child has an `Inherit` option, and the parent is `Set`, then the
    child will inherit the parent's `Set` value.
3.  The only other case is a pair of `Inherit` options, which will result in
    an `Inherit` outcome.

-}
combineOption : Option a -> Option a -> Option a
combineOption parent child =
    case ( parent, child ) of
        ( _, Set childValue ) ->
            Set childValue

        ( Set parentValue, Inherit ) ->
            Set parentValue

        ( Inherit, Inherit ) ->
            Inherit



---- Style Setting Functions --------------------------------------------------


{-| The style that inherits everything and sets nothing.

This is the default style.

-}
inheritAll : Style
inheritAll =
    Style
        { fill = fillInheritAll
        , stroke = strokeInheritAll
        }


fillInheritAll : Fill
fillInheritAll =
    Fill
        { fill = Inherit
        , fillRule = Inherit
        }


strokeInheritAll : Stroke
strokeInheritAll =
    Stroke
        { stroke = Inherit
        , strokeWidth = Inherit
        , lineCap = Inherit
        , lineJoin = Inherit
        , dashArray = Inherit
        , dashOffset = Inherit
        }


{-| Modify the `fill` setting of a `Style`.
-}
styleModifyFill : (Fill -> Fill) -> Style -> Style
styleModifyFill fillFn (Style input) =
    { input | fill = fillFn input.fill } |> Style


{-| Modify the `stroke` setting of a `Style`.
-}
styleModifyStroke : (Stroke -> Stroke) -> Style -> Style
styleModifyStroke strokeFn (Style input) =
    { input | stroke = strokeFn input.stroke } |> Style


{-| Set the fill paint.
-}
fill : Paint -> Style -> Style
fill paint =
    styleModifyFill <| \(Fill phil) -> Fill { phil | fill = Set paint }


{-| Set the fill rule.
-}
fillRule : FillRule -> Style -> Style
fillRule fr =
    styleModifyFill <| \(Fill phil) -> Fill { phil | fillRule = Set fr }


{-| Set the stroke paint.
-}
stroke : Paint -> Style -> Style
stroke paint =
    styleModifyStroke <| \(Stroke st) -> Stroke { st | stroke = Set paint }


{-| Set the stroke width.
-}
strokeWidth : Float -> Style -> Style
strokeWidth w =
    styleModifyStroke <| \(Stroke st) -> Stroke { st | strokeWidth = Set w }


{-| Set the line cap.
-}
lineCap : LineCap -> Style -> Style
lineCap c =
    styleModifyStroke <| \(Stroke st) -> Stroke { st | lineCap = Set c }


{-| Set the line join.
-}
lineJoin : LineJoin -> Style -> Style
lineJoin j =
    styleModifyStroke <| \(Stroke st) -> Stroke { st | lineJoin = Set j }


{-| Set the dash array.
-}
dashArray : DashArray -> Style -> Style
dashArray d =
    styleModifyStroke <| \(Stroke st) -> Stroke { st | dashArray = Set d }


{-| Set the dash offset.
-}
dashOffset : Float -> Style -> Style
dashOffset o =
    styleModifyStroke <| \(Stroke st) -> Stroke { st | dashOffset = Set o }



---- Hashing ------------------------------------------------------------------


{-| Hash an `UnhashedLinearGradient`.
-}
hashUnhashedLinearGradient : UnhashedLinearGradient -> SHA1.Digest
hashUnhashedLinearGradient =
    encLinearGradient >> Encode.encode >> SHA1.fromBytes


{-| Hash an `UnhashedRadialGradient`.
-}
hashUnhashedRadialGradient : UnhashedRadialGradient -> SHA1.Digest
hashUnhashedRadialGradient =
    encRadialGradient >> Encode.encode >> SHA1.fromBytes


{-| Hash an `UnhashedGradient`.
-}
hashUnhashedGradient : UnhashedGradient -> SHA1.Digest
hashUnhashedGradient =
    encGradient >> Encode.encode >> SHA1.fromBytes


{-| Encode a `RadialGradient`. Only for hashing.

This is not a complete encoding, because it encodes the hash of the gradient.

-}
encRadialGradient : UnhashedRadialGradient -> Encoder
encRadialGradient (UnhashedRadialGradient g) =
    Encode.sequence
        [ Math.encP2 g.innerCenter
        , Codec.encf32 g.innerRadius
        , Math.encP2 g.outerCenter
        , Codec.encf32 g.outerRadius
        , Math.encAffineTransform g.transform
        , encSHA1Digest <| gradientSHA1Digest <| g.gradient
        ]


{-| Encode a `LinearGradient`. Only for hashing.

This is not a complete encoding, because it encodes the hash of the gradient.

-}
encLinearGradient : UnhashedLinearGradient -> Encoder
encLinearGradient (UnhashedLinearGradient g) =
    Encode.sequence
        [ Math.encP2 g.start
        , Math.encP2 g.end
        , Math.encAffineTransform g.transform
        , encSHA1Digest <| gradientSHA1Digest <| g.gradient
        ]


{-| Encode a `SHA1.Digest`. Only for hashing.
-}
encSHA1Digest : SHA1.Digest -> Encoder
encSHA1Digest digest =
    let
        d =
            SHA1.toInt32s digest
    in
    Encode.sequence
        [ Codec.encu32 d.a
        , Codec.encu32 d.b
        , Codec.encu32 d.c
        , Codec.encu32 d.d
        , Codec.encu32 d.e
        ]


{-| Encode an `UnhashedGradient`. Only for hashing.
-}
encGradient : UnhashedGradient -> Encoder
encGradient (UnhashedGradient stops) =
    Codec.encList encStop stops


{-| Encode a `Stop`. Only for hashing.
-}
encStop : Stop -> Encoder
encStop (Stop location color) =
    Encode.sequence
        [ Codec.encf32 location
        , Codec.encColor color
        ]
