module Techdraw.Style exposing
    ( Style(..)
    , Option(..)
    , Fill(..)
    , FillRule(..)
    , Stroke(..)
    , LineCap, LineJoin, DashArray
    , Paint(..)
    , LinearGradient(..), RadialGradient(..)
    , Gradient, Stop(..), gradient
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

@docs LinearGradient, RadialGradient
@docs Gradient, Stop, gradient


# Operations

@docs combineStyle


# Style Setting Functions

@docs inheritAll
@docs fill, fillRule
@docs stroke, strokeWidth, lineCap, lineJoin, dashArray, dashOffset

-}

import Color exposing (Color)
import Techdraw.Math exposing (AffineTransform, P2)



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


{-| Linear gradient.

A linear gradient is specified by:

  - `start`: a start point.
  - `end`: an end point.
  - `transform`: additional gradient transform, which maps from the gradient
    coordinate system to the local coordinate system.
  - `gradient`: the list of gradient stops.

-}
type LinearGradient
    = LinearGradient
        { start : P2
        , end : P2
        , transform : AffineTransform
        , gradient : Gradient
        }


{-| Radial gradient.

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
type RadialGradient
    = RadialGradient
        { innerCenter : P2
        , innerRadius : Float
        , outerCenter : P2
        , outerRadius : Float
        , transform : AffineTransform
        , gradient : Gradient
        }


{-| Gradient.

A gradient is a list of stops sorted by location.
Use the [`gradient`](#gradient) smart constructor to create a `Gradient`.

-}
type Gradient
    = Gradient (List Stop)


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
gradient =
    Gradient << List.sortBy getStopLocation



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
