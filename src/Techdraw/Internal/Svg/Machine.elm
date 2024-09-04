module Techdraw.Internal.Svg.Machine exposing
    ( ToSvgResult
    , svgResultGetHtml, svgResultGetWarnings, svgResultTuple
    , toSvg
    )

{-| Drawing state machine for producing SVG.

This module implements a largely non-recursive state machine to evaluate a
`Dwg msg` into an `Svg msg`. Recursion is avoided for stack safety.


# Result Type and Warnings

@docs ToSvgResult
@docs svgResultGetHtml, svgResultGetWarnings, svgResultTuple


# Conversion

@docs toSvg

-}

import Html exposing (Attribute, Html)
import Html.Attributes as HtmlA
import Techdraw.Internal.Dwg exposing (Dwg(..))
import Techdraw.Internal.StyleAtom as StyleAtom
import Techdraw.Internal.Svg.Defs as Defs
import Techdraw.Internal.Svg.Env as Env exposing (Env, Warning)
import Techdraw.Internal.Svg.Path as SvgPath
import Techdraw.Internal.Svg.Style as SvgStyle
import Techdraw.Path as Path exposing (Path)
import Techdraw.Types as T exposing (ContainerSize, Sizing, ViewBox)
import TypedSvg
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)



---- Result Type and Warnings -------------------------------------------------


{-| Result of the `toSvg` function.

This collects together the `Html` produced (containing an `<svg>` element),
and a list of any warnings produced along the way.

-}
type ToSvgResult msg
    = ToSvgResult (Html msg) (List Warning)


{-| Return the HTML from a `ToSvgResult`.
-}
svgResultGetHtml : ToSvgResult msg -> Html msg
svgResultGetHtml (ToSvgResult html _) =
    html


{-| Return the list of warnings from a `ToSvgResult`.
-}
svgResultGetWarnings : ToSvgResult msg -> List Warning
svgResultGetWarnings (ToSvgResult _ warnings) =
    warnings


{-| Return a tuple version of `ToSvgResult`.
-}
svgResultTuple : ToSvgResult msg -> ( Html msg, List String )
svgResultTuple result =
    ( svgResultGetHtml result
    , svgResultGetWarnings result |> List.map Env.unWarning
    )



---- Conversion ---------------------------------------------------------------


{-| Convert a drawing to SVG.
-}
toSvg : Sizing -> Dwg msg -> ToSvgResult msg
toSvg sizing =
    inject sizing >> loopTailRecursive >> produceHtml sizing


{-| Produce final HTML after running the state machine to completion.
-}
produceHtml : Sizing -> StateMachineResult msg -> ToSvgResult msg
produceHtml sizing (StateMachineResult svgs warnings) =
    ToSvgResult (TypedSvg.svg (sizingToSvg sizing) svgs) warnings


{-| Convert all `Sizing` parameters to attributes on an SVG element.
-}
sizingToSvg : Sizing -> List (Attribute msg)
sizingToSvg sizing =
    (T.sizingGetContainerSize sizing |> containerSizeToSvg)
        ++ (T.sizingGetViewBox sizing |> viewBoxToSvg)


{-| Convert a `ContainerSize` to HTML height and width attributes.
-}
containerSizeToSvg : ContainerSize -> List (Attribute msg)
containerSizeToSvg cs =
    [ HtmlA.width (T.containerSizeGetWidth cs)
    , HtmlA.height (T.containerSizeGetHeight cs)
    ]


{-| Convert a `ViewBox` to an SVG viewbox.
-}
viewBoxToSvg : ViewBox -> List (Attribute msg)
viewBoxToSvg vb =
    [ SvgA.viewBox
        (T.viewBoxGetXMin vb)
        (T.viewBoxGetYMin vb)
        (T.viewBoxGetWidth vb)
        (T.viewBoxGetHeight vb)
    ]


{-| Tail-recursive loop which steps the state machine evaluator until it
has completed and produces a result.
-}
loopTailRecursive : State msg -> StateMachineResult msg
loopTailRecursive state =
    case extractIfDone state of
        Just result ->
            result

        Nothing ->
            let
                newState =
                    step state
            in
            loopTailRecursive newState



---- State Machine ------------------------------------------------------------


{-| Final result of running the state machine.
-}
type StateMachineResult msg
    = StateMachineResult (List (Svg msg)) (List Warning)


{-| Expression in the state machine.

An expression is either:

  - An initial drawing value, or
  - A final SVG value.

-}
type Expr msg
    = Init (Dwg msg)
    | Fine (List (Svg msg))


{-| Continuation for the state machine.
-}
type Kont msg
    = KontNothing


{-| State in the state machine.
-}
type alias State msg =
    { expr : Expr msg
    , envr : Env msg
    , kont : List (Kont msg)
    }


{-| Final SVG value, with an empty list of SVGs.
-}
emptyFine : Expr msg
emptyFine =
    Fine []


{-| Inject an initial size and drawing into the machine state.
-}
inject : Sizing -> Dwg msg -> State msg
inject sizing dwg =
    { expr = Init dwg
    , envr = Env.init sizing
    , kont = []
    }


{-| If the state machine has finished, return its result.
-}
extractIfDone : State msg -> Maybe (StateMachineResult msg)
extractIfDone state =
    case ( state.expr, state.kont ) of
        ( Fine svgs, [] ) ->
            Just <|
                prependDefs state <|
                    StateMachineResult svgs (Env.getWarnings state.envr)

        _ ->
            Nothing


{-| Convert `Defs` from the environment into a `<defs>` SVG element and
prepend it onto the `StateMachineResult`.
-}
prependDefs : State msg -> StateMachineResult msg -> StateMachineResult msg
prependDefs state (StateMachineResult svgs warnings) =
    StateMachineResult
        (Defs.toSvg (Env.getDefs state.envr) :: svgs)
        warnings


{-| Step the state machine.
-}
step : State msg -> State msg
step state =
    case ( state.expr, state.kont ) of
        {- ----------------------------------------------------------------- -}
        {- Terminal state -}
        {- ----------------------------------------------------------------- -}
        ( Fine _, [] ) ->
            state

        {- ----------------------------------------------------------------- -}
        {- Initial states -}
        {- ----------------------------------------------------------------- -}
        {- Empty drawing. -}
        ( Init DwgEmpty, _ ) ->
            state |> setExpr (Fine [])

        {- Draw a Path. -}
        ( Init (DwgPath path), _ ) ->
            let
                ( sAttrs, state1 ) =
                    styleAttrs state
            in
            state1 |> setExpr (Fine (pathToSvg state.envr sAttrs path))

        {- Update the style in the environment. -}
        ( Init (DwgStyled atom drawing), _ ) ->
            state
                |> setExpr (Init drawing)
                |> modEnvr (Env.applyStyleAtom atom)

        {- ----------------------------------------------------------------- -}
        {- Continuation states -}
        {- ----------------------------------------------------------------- -}
        ( Fine _, KontNothing :: ks ) ->
            { state | kont = ks }


{-| Set the expression in the state.
-}
setExpr : Expr msg -> State msg -> State msg
setExpr e oldState =
    { oldState | expr = e }


{-| Modify the environment inside the state.
-}
modEnvr : (Env msg -> Env msg) -> State msg -> State msg
modEnvr envModFn oldState =
    { oldState | envr = envModFn oldState.envr }


{-| Get the style attributes for the current state's style.
-}
styleAttrs : State msg -> ( List (Attribute msg), State msg )
styleAttrs state =
    let
        ( attrs, childEnv ) =
            SvgStyle.envStyleToSvg state.envr
    in
    ( attrs, { state | envr = Env.thread state.envr childEnv } )


{-| Convert a `Path` to SVG in the current environment.

TODO: Styling

-}
pathToSvg : Env msg -> List (Attribute msg) -> Path -> List (Svg msg)
pathToSvg env sAttrs path =
    let
        l2w =
            Env.getLocalToWorld env

        nfd =
            Env.getNFixDigits env

        str =
            Path.pathApplyAffineTransform l2w path |> SvgPath.toString nfd

        attrs =
            SvgA.d str :: sAttrs
    in
    [ TypedSvg.path attrs [] ]
