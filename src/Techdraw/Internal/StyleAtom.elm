module Techdraw.Internal.StyleAtom exposing (StyleAtom(..), apply)

{-| Individual components of a Style.

[`StyleAtom`](#StyleAtom) is used so that style settings in a drawing tree
take up as little room as possible.

@docs StyleAtom, apply

-}

import Techdraw.Style as Style exposing (Style)


{-| Individual component of a `Style`.
-}
type StyleAtom
    = Fill Style.Paint
    | FillRule Style.FillRule
    | Stroke Style.Paint
    | StrokeWidth Float
    | LineCap Style.LineCap
    | LineJoin Style.LineJoin
    | DashArray Style.DashArray
    | DashOffset Float


{-| Apply a `StyleAtom` to a style, producing a new style.
-}
apply : StyleAtom -> Style -> Style
apply styleAtom =
    case styleAtom of
        Fill paint ->
            Style.fill paint

        FillRule fillRule ->
            Style.fillRule fillRule

        Stroke paint ->
            Style.stroke paint

        StrokeWidth strokeWidth ->
            Style.strokeWidth strokeWidth

        LineCap lineCap ->
            Style.lineCap lineCap

        LineJoin lineJoin ->
            Style.lineJoin lineJoin

        DashArray dashArray ->
            Style.dashArray dashArray

        DashOffset dashOffset ->
            Style.dashOffset dashOffset
