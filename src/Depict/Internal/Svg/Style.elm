module Depict.Internal.Svg.Style exposing (envStyleToSvg)

{-| Styling in SVG.

@docs envStyleToSvg

-}

import Depict.Internal.Hash as Hash
import Depict.Internal.Svg.Defs as Defs
import Depict.Internal.Svg.Env as Env exposing (Env)
import Depict.Style as Style
    exposing
        ( Fill(..)
        , LinearGradient
        , Option(..)
        , Paint(..)
        , RadialGradient
        , Stroke(..)
        , Style(..)
        )
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Core exposing (Attribute, attribute)
import TypedSvg.Types as SvgTypes exposing (px)


{-| Convert the current `Style` to SVG.
-}
envStyleToSvg : Env msg -> ( List (Attribute msg), Env msg )
envStyleToSvg env =
    let
        (Style style) =
            Env.getStyle env

        ( fillAttr, env1 ) =
            fillToSvg env style.fill

        ( strokeAttr, env2 ) =
            strokeToSvg env1 style.stroke
    in
    ( fillAttr ++ strokeAttr, env2 )


{-| Convert the `Fill` to SVG.
-}
fillToSvg : Env msg -> Fill -> ( List (Attribute msg), Env msg )
fillToSvg env (Fill fill) =
    let
        ( fillAttr, newEnv ) =
            optionPaintToAttr env SvgAttr.fill fill.fill
    in
    ( List.concat
        [ fillAttr
        , optionToAttr (translateFillRule >> SvgAttr.fillRule) fill.fillRule
        ]
    , newEnv
    )


{-| Convert the `Stroke` to SVG.
-}
strokeToSvg : Env msg -> Stroke -> ( List (Attribute msg), Env msg )
strokeToSvg env (Stroke stroke) =
    let
        ( strokeAttr, newEnv ) =
            optionPaintToAttr env SvgAttr.stroke stroke.stroke
    in
    ( List.concat
        [ strokeAttr
        , optionToAttr (px >> SvgAttr.strokeWidth) stroke.strokeWidth
        , optionToAttr
            (translateLineCap >> SvgAttr.strokeLinecap)
            stroke.lineCap
        , lineJoinToAttrs stroke.lineJoin
        , optionToAttr
            (unDashArray
                >> List.map String.fromFloat
                >> String.join " "
                >> SvgAttr.strokeDasharray
            )
            stroke.dashArray
        , optionToAttr
            (String.fromFloat >> SvgAttr.strokeDashoffset)
            stroke.dashOffset
        ]
    , newEnv
    )


{-| Convert `LineJoin` to a list of attributes.

  - We need to include the miter limit here.
  - Some options are missing from TypedSvg.

-}
lineJoinToAttrs : Option Style.LineJoin -> List (Attribute msg)
lineJoinToAttrs optLineJoin =
    case optLineJoin of
        Inherit ->
            []

        Set (Style.LineJoinMiter limit) ->
            [ SvgAttr.strokeLinejoin SvgTypes.StrokeLinejoinMiter
            , SvgAttr.strokeMiterlimit (String.fromFloat limit)
            ]

        Set (Style.LineJoinMiterClip limit) ->
            [ attribute "stroke-linejoin" "miter-clip"
            , SvgAttr.strokeMiterlimit (String.fromFloat limit)
            ]

        Set Style.LineJoinRound ->
            [ SvgAttr.strokeLinejoin SvgTypes.StrokeLinejoinRound ]

        Set Style.LineJoinBevel ->
            [ SvgAttr.strokeLinejoin SvgTypes.StrokeLinejoinBevel ]

        Set Style.LineJoinArcs ->
            [ attribute "stroke-linejoin" "arcs" ]


{-| Translate the `FillRule` to SVG.
-}
translateFillRule : Style.FillRule -> SvgTypes.FillRule
translateFillRule fr =
    case fr of
        Style.NonZero ->
            SvgTypes.FillRuleNonZero

        Style.EvenOdd ->
            SvgTypes.FillRuleEvenOdd


{-| Translate `LineCap` to SVG.
-}
translateLineCap : Style.LineCap -> SvgTypes.StrokeLinecap
translateLineCap lc =
    case lc of
        Style.LineCapButt ->
            SvgTypes.StrokeLinecapButt

        Style.LineCapRound ->
            SvgTypes.StrokeLinecapRound

        Style.LineCapSquare ->
            SvgTypes.StrokeLinecapSquare


{-| Convert an Option to an attribute.
-}
optionToAttr : (a -> Attribute msg) -> Option a -> List (Attribute msg)
optionToAttr toAttr optValue =
    optionToMaybe optValue
        |> Maybe.map (\x -> [ toAttr x ])
        |> Maybe.withDefault []


{-| Convert an `Option Paint` to an SVG attribute.
-}
optionPaintToAttr :
    Env msg
    -> (SvgTypes.Paint -> Attribute msg)
    -> Option Paint
    -> ( List (Attribute msg), Env msg )
optionPaintToAttr env toAttr optPaint =
    optionToMaybe optPaint
        |> Maybe.map (paintToAttr env toAttr)
        |> Maybe.withDefault ( [], env )


{-| Convert a `Paint` to an SVG attribute.
-}
paintToAttr :
    Env msg
    -> (SvgTypes.Paint -> Attribute msg)
    -> Paint
    -> ( List (Attribute msg), Env msg )
paintToAttr env toAttr paint =
    let
        ( svgPaint, newEnv ) =
            paintToSvg env paint
    in
    ( [ toAttr svgPaint ], newEnv )


{-| Convert a `Paint` value to SVG.

Any gradients have to be transformed using the current local-to-world
transformation. Additionally, the environment must be updated so that the
`<defs>` element will contain the gradients.

-}
paintToSvg : Env msg -> Paint -> ( SvgTypes.Paint, Env msg )
paintToSvg env paint =
    case paint of
        Paint color ->
            ( SvgTypes.Paint color, env )

        PaintLinearGradient lg ->
            linearGradientToSvg env lg

        PaintRadialGradient rg ->
            radialGradientToSvg env rg


{-| Convert a `LinearGradient` to SVG.

We have to transform the gradient using the current local-to-world
transformation. The transformation is applied to the gradient's
`gradientTransform` attribute, leaving everything else alone.

The environment is updated so that the `<defs>` element will contain
the gradient. The gradient's ID is its hash.

-}
linearGradientToSvg : Env msg -> LinearGradient -> ( SvgTypes.Paint, Env msg )
linearGradientToSvg env lg =
    let
        transformedGradient =
            Style.linearGradientApplyAffineTransform
                (Env.getLocalToWorld env)
                lg

        svgPaint =
            SvgTypes.Reference <|
                Hash.toHex <|
                    Style.hashLinearGradient transformedGradient

        newEnv =
            Env.modDefs (Defs.ensureLinearGradient transformedGradient) env
    in
    ( svgPaint, newEnv )


{-| Convert a `RadialGradient` to SVG.

We have to transform the gradient using the current local-to-world
transformation. The transformation is applied to the gradient's
`gradientTransform` attribute, leaving everything else alone.

The environment is updated so that the `<defs>` element will contain
the gradient. The gradient's ID is its hash.

-}
radialGradientToSvg : Env msg -> RadialGradient -> ( SvgTypes.Paint, Env msg )
radialGradientToSvg env rg =
    let
        transformedGradient =
            Style.radialGradientApplyAffineTransform
                (Env.getLocalToWorld env)
                rg

        svgPaint =
            SvgTypes.Reference <|
                Hash.toHex <|
                    Style.hashRadialGradient transformedGradient

        newEnv =
            Env.modDefs (Defs.ensureRadialGradient transformedGradient) env
    in
    ( svgPaint, newEnv )


{-| Convert a styling option to a `Maybe`.
-}
optionToMaybe : Option a -> Maybe a
optionToMaybe option =
    case option of
        Inherit ->
            Nothing

        Set value ->
            Just value


{-| Unapply the dasharray constructor.
-}
unDashArray : Style.DashArray -> List Float
unDashArray (Style.DashArray fs) =
    fs
