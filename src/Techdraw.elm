module Techdraw exposing
    ( Drawing
    , render
    , empty, path, svg, group, transform
    , style
    , fill, stroke, strokeWidth
    , Style
    , styleDefault, styleInheritAll
    , styleSetFill, styleSetStroke, styleSetStrokeWidth
    , styleGetFill, styleGetStroke, styleGetStrokeWidth
    , styleAppendDecorator, styleAppendAttribute
    , styleGetDecorators, styleGetAttributes
    , Decorator(..)
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

@docs style
@docs fill, stroke, strokeWidth


# Styles

@docs Style
@docs styleDefault, styleInheritAll
@docs styleSetFill, styleSetStroke, styleSetStrokeWidth
@docs styleGetFill, styleGetStroke, styleGetStrokeWidth
@docs styleAppendDecorator, styleAppendAttribute
@docs styleGetDecorators, styleGetAttributes


# Path Decoration

@docs Decorator

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
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint, px)
import VirtualDom exposing (Attribute)



---- Drawing ------------------------------------------------------------------


{-| Drawing.
-}
type Drawing msg
    = DrawingEmpty
    | DrawingPath Path
    | DrawingSvg (Style msg -> AffineTransform -> Svg msg)
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

The function provided takes the current style and local-to-world
transformation so that it can produce custom SVG.

-}
svg : (Style msg -> AffineTransform -> Svg msg) -> Drawing msg
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
style : Style msg -> Drawing msg -> Drawing msg
style parentStyle drawing =
    case drawing of
        DrawingStyled childStyle dwg ->
            -- Any existing child style applied to the drawing takes
            -- precedence.
            DrawingStyled (combineStyles parentStyle childStyle) dwg

        _ ->
            DrawingStyled parentStyle drawing


{-| Set the fill.

See [`style`](#style) for information on precedence.

-}
fill : Paint -> Drawing msg -> Drawing msg
fill paint drawing =
    style (styleDefault |> styleSetFill paint) drawing


{-| Set the stroke.

See [`style`](#style) for information on precedence.

-}
stroke : Paint -> Drawing msg -> Drawing msg
stroke paint drawing =
    style (styleDefault |> styleSetStroke paint) drawing


{-| Set the stroke width.

See [`style`](#style) for information on precedence.

-}
strokeWidth : Float -> Drawing msg -> Drawing msg
strokeWidth width drawing =
    style (styleDefault |> styleSetStrokeWidth width) drawing



---- Styles -------------------------------------------------------------------


{-| Indicate whether a style setting is inherited or explicitly set.
-}
type StyleSetting a
    = Inherited
    | Set a


{-| Convert a `StyleSetting` to a `Maybe` value.
-}
styleSettingToMaybe : StyleSetting a -> Maybe a
styleSettingToMaybe setting =
    case setting of
        Inherited ->
            Nothing

        Set value ->
            Just value


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
        , decorators : List (Decorator msg)
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
        , decorators = []
        , extraAttributes = []
        }


{-| Set the fill.
-}
styleSetFill : Paint -> Style msg -> Style msg
styleSetFill paint (Style styl) =
    Style <| { styl | fill = Set paint }


{-| Return the fill setting.
-}
styleGetFill : Style msg -> Maybe Paint
styleGetFill (Style styl) =
    styl.fill |> styleSettingToMaybe


{-| Set the stroke.
-}
styleSetStroke : Paint -> Style msg -> Style msg
styleSetStroke paint (Style styl) =
    Style <| { styl | stroke = Set paint }


{-| Return the stroke setting.
-}
styleGetStroke : Style msg -> Maybe Paint
styleGetStroke (Style styl) =
    styl.stroke |> styleSettingToMaybe


{-| Set the stroke width.
-}
styleSetStrokeWidth : Float -> Style msg -> Style msg
styleSetStrokeWidth width (Style styl) =
    Style <| { styl | strokeWidth = Set width }


{-| Return the stroke width.
-}
styleGetStrokeWidth : Style msg -> Maybe Float
styleGetStrokeWidth (Style styl) =
    styl.strokeWidth |> styleSettingToMaybe


{-| Append a decorator.
-}
styleAppendDecorator : Decorator msg -> Style msg -> Style msg
styleAppendDecorator decorator (Style styl) =
    Style <| { styl | decorators = decorator :: styl.decorators }


{-| Return the decorators.

The decorators are returned in the sequence they should be applied.

-}
styleGetDecorators : Style msg -> List (Decorator msg)
styleGetDecorators (Style styl) =
    styl.decorators |> List.reverse


{-| Append a custom SVG attribute.

Typically this is used for setting styling attributes that are not directly
supported by the library. However, any provided attribute will be appended
to the output SVG.

-}
styleAppendAttribute : Attribute msg -> Style msg -> Style msg
styleAppendAttribute attribute (Style styl) =
    Style <| { styl | extraAttributes = attribute :: styl.extraAttributes }


{-| Return the attributes.

The attributes are returned in the sequence they should be applied.

-}
styleGetAttributes : Style msg -> List (Attribute msg)
styleGetAttributes (Style styl) =
    styl.extraAttributes |> List.reverse


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
        , decorators = child.decorators ++ parent.decorators
        , extraAttributes = child.extraAttributes ++ parent.extraAttributes
        }



---- Path Decoration ----------------------------------------------------------


{-| A decorator is a special styling operation that can operate on a
processed path.

A decorator runs on a path after:

  - A collapsed style has been computed (taking into account the styles
    of groups above the path, etc)
  - The local-to-world transformation has been computed.
  - The path has been transformed to world space.

A `Decorator` can then modify the path as it sees fit, to produce a new
[`Drawing`](#Drawing). The original `Drawing` is discarded, so if the
aim is to retain it, then it must be part of the `Drawing` returned by
the `Decorator`.

-}
type Decorator msg
    = Decorator
        (Style msg
         -> AffineTransform
         -> Path
         -> Drawing msg
        )



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
    let
        styl : Style msg
        styl =
            state |> stateStyle

        localToWorld : AffineTransform
        localToWorld =
            state |> stateLocalToWorld

        decorators : List (Decorator msg)
        decorators =
            styl |> styleGetDecorators
    in
    case drawing of
        DrawingEmpty ->
            g [] []

        DrawingPath pth ->
            if List.isEmpty decorators then
                -- The style has no decorators. This means the path has no
                -- further processing required and will be dumped to SVG
                -- directly.
                let
                    styleAttributes =
                        styleToAttributes styl

                    pathAttr =
                        affApplyPath localToWorld pth |> toSvgPPath |> SvgAttributes.d

                    attributes =
                        pathAttr :: styleAttributes
                in
                TypedSvg.path attributes []

            else
                -- The style has decorators, so we must process them to
                -- produce a new drawing. At this point, we strip out all
                -- the previous transformations and style, since the
                -- decorator has access to those directly.
                let
                    xfpath =
                        affApplyPath localToWorld pth
                in
                List.map (\(Decorator f) -> f styl localToWorld xfpath) decorators
                    |> group
                    |> renderWithState initState

        DrawingSvg mkSvgChild ->
            mkSvgChild styl localToWorld

        DrawingStyled parentStyle childDrawing ->
            renderWithState (stateCombineStyles state parentStyle) childDrawing

        DrawingTransformed childToParent childDrawing ->
            renderWithState (stateComposeTransform state childToParent) childDrawing

        DrawingGroup children ->
            g [] <| List.map (renderWithState state) children


{-| Convert a style to a list of SVG attributes.
-}
styleToAttributes : Style msg -> List (Attribute msg)
styleToAttributes styl =
    let
        toAttr : (Style msg -> Maybe a) -> (a -> Attribute msg) -> Maybe (Attribute msg)
        toAttr extract produce =
            extract styl |> Maybe.map produce

        styleAttrs =
            List.filterMap (\x -> x)
                [ toAttr styleGetFill SvgAttributes.fill
                , toAttr styleGetStroke SvgAttributes.stroke
                , toAttr styleGetStrokeWidth (\x -> SvgAttributes.strokeWidth (px x))
                ]
    in
    styleAttrs ++ List.reverse (styleGetAttributes styl)
