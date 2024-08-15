module Techdraw exposing
    ( Drawing
    , render
    , empty, path, svg, group, transform
    , withStyle
    , withFill, withStroke, withStrokeWidth
    , Style
    , styleDefault, styleInheritAll
    , fill, stroke, strokeWidth
    , appendAttribute
    )

{-|


# Drawing


## Types

@docs Drawing


## Creating SVG

@docs render


## Creating Drawings


### First-class Operations

@docs empty, path, svg, group, transform


### Derived Operations

TODO:

  - Rectangle
  - Circle


## Styling drawings

@docs withStyle
@docs withFill, withStroke, withStrokeWidth


# Styles

@docs Style
@docs styleDefault, styleInheritAll
@docs fill, stroke, strokeWidth
@docs appendAttribute

-}

import Techdraw.Math as Math
    exposing
        ( AffineTransform(..)
        , Path
        , affApplyPath
        , affIdentity
        , affMul
        , toSvgPPath
        )
import TypedSvg exposing (g)
import TypedSvg.Attributes exposing (d)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint)
import VirtualDom exposing (Attribute)



---- Drawing ------------------------------------------------------------------


{-| Drawing.
-}
type Drawing msg
    = DrawingEmpty
    | DrawingPath Path
    | DrawingSvg (Svg msg)
    | DrawingStyled (Style msg) (Drawing msg)
    | DrawingTransformed AffineTransform (Drawing msg)
    | DrawingGroup (List (Drawing msg))


{-| Empty drawing.
-}
empty : Drawing msg
empty =
    DrawingEmpty


{-| Draw a path.
-}
path : Path -> Drawing msg
path =
    DrawingPath


{-| Draw some SVG directly.
-}
svg : Svg msg -> Drawing msg
svg =
    DrawingSvg


{-| Transform a drawing.
-}
transform : AffineTransform -> Drawing msg -> Drawing msg
transform parentTransform drawing =
    case drawing of
        DrawingTransformed childTransform childDrawing ->
            DrawingTransformed (Math.affMul parentTransform childTransform) childDrawing

        _ ->
            DrawingTransformed parentTransform drawing


{-| Group drawings.
-}
group : List (Drawing msg) -> Drawing msg
group =
    DrawingGroup


{-| Set the style of a drawing.

The style is applied "on the outside". Any styles already defined will take
precedence.

-}
withStyle : Style msg -> Drawing msg -> Drawing msg
withStyle parentStyle drawing =
    case drawing of
        DrawingStyled childStyle dwg ->
            -- Any existing child style applied to the drawing takes
            -- precedence.
            DrawingStyled (combineStyles parentStyle childStyle) dwg

        _ ->
            DrawingStyled parentStyle drawing


{-| Set the fill.

See [`withStyle`](#withStyle) for information on precedence.

-}
withFill : Paint -> Drawing msg -> Drawing msg
withFill paint drawing =
    withStyle (styleDefault |> fill paint) drawing


{-| Set the stroke.

See [`withStyle`](#withStyle) for information on precedence.

-}
withStroke : Paint -> Drawing msg -> Drawing msg
withStroke paint drawing =
    withStyle (styleDefault |> stroke paint) drawing


{-| Set the stroke width.

See [`withStyle`](#withStyle) for information on precedence.

-}
withStrokeWidth : Float -> Drawing msg -> Drawing msg
withStrokeWidth width drawing =
    withStyle (styleDefault |> strokeWidth width) drawing



---- Styles -------------------------------------------------------------------


{-| Indicate whether a style setting is inherited or explicitly set.
-}
type StyleSetting a
    = Inherited
    | Set a


{-| Combine a style setting from a parent and child into a new current style
setting.
-}
combineStyleSetting : StyleSetting a -> StyleSetting a -> StyleSetting a
combineStyleSetting parent child =
    case ( parent, child ) of
        ( _, Set value ) ->
            Set value

        ( Set value, Inherited ) ->
            Set value

        ( Inherited, Inherited ) ->
            Inherited


{-| Style.
-}
type Style msg
    = Style
        { fill : StyleSetting Paint
        , stroke : StyleSetting Paint
        , strokeWidth : StyleSetting Float
        , extraAttributes : List (Attribute msg)
        }


{-| Default style.

This is a synonym for [`styleInheritAll`](#styleInheritAll), which inherits
everything from its parent.

-}
styleDefault : Style msg
styleDefault =
    styleInheritAll


{-| A style which inherits everything from its parent.
-}
styleInheritAll : Style msg
styleInheritAll =
    Style
        { fill = Inherited
        , stroke = Inherited
        , strokeWidth = Inherited
        , extraAttributes = []
        }


{-| Set the fill.
-}
fill : Paint -> Style msg -> Style msg
fill paint (Style style) =
    Style <| { style | fill = Set paint }


{-| Set the stroke.
-}
stroke : Paint -> Style msg -> Style msg
stroke paint (Style style) =
    Style <| { style | stroke = Set paint }


{-| Set the stroke width.
-}
strokeWidth : Float -> Style msg -> Style msg
strokeWidth width (Style style) =
    Style <| { style | strokeWidth = Set width }


{-| Append a custom SVG attribute.

Typically this is used for setting styling attributes that are not directly
supported by the library. However, any provided attribute will be appended
to the output SVG.

-}
appendAttribute : Attribute msg -> Style msg -> Style msg
appendAttribute attribute (Style style) =
    Style <| { style | extraAttributes = attribute :: style.extraAttributes }


{-| Combine settings from a parent style and child style into a new
current style.
-}
combineStyles : Style msg -> Style msg -> Style msg
combineStyles (Style parent) (Style child) =
    let
        comb extract =
            combineStyleSetting (extract parent) (extract child)
    in
    Style
        { fill = comb .fill
        , stroke = comb .stroke
        , strokeWidth = comb .strokeWidth
        , extraAttributes = child.extraAttributes ++ parent.extraAttributes
        }



---- Rendering ----------------------------------------------------------------


{-| Render a diagram to SVG.
-}
render : Drawing msg -> Svg msg
render =
    renderWithState initState


type State msg
    = State
        { style : Style msg
        , localToWorld : AffineTransform
        }


initState : State msg
initState =
    State
        { style = styleDefault
        , localToWorld = affIdentity
        }


stateStyle : State msg -> Style msg
stateStyle (State state) =
    state.style


stateLocalToWorld : State msg -> AffineTransform
stateLocalToWorld (State state) =
    state.localToWorld


stateCombineStyles : State msg -> Style msg -> State msg
stateCombineStyles (State parent) child =
    State { parent | style = combineStyles parent.style child }


stateComposeTransform : State msg -> AffineTransform -> State msg
stateComposeTransform (State parent) child =
    State { parent | localToWorld = affMul parent.localToWorld child }


{-| Render a diagram as SVG.

This is a recursive evaluator for now. If necessary, it can be converted to
use something like a CEK machine.

-}
renderWithState : State msg -> Drawing msg -> Svg msg
renderWithState state drawing =
    case drawing of
        DrawingEmpty ->
            g [] []

        DrawingPath pth ->
            TypedSvg.path
                [ -- Transform the path and convert it to SVG
                  affApplyPath (stateLocalToWorld state) pth
                    |> toSvgPPath
                    |> d

                -- TODO: Add styles here.
                ]
                []

        DrawingSvg svgChild ->
            svgChild

        DrawingStyled style childDrawing ->
            renderWithState (stateCombineStyles state style) childDrawing

        DrawingTransformed childToParent childDrawing ->
            renderWithState (stateComposeTransform state childToParent) childDrawing

        DrawingGroup children ->
            g [] <| List.map (renderWithState state) children
