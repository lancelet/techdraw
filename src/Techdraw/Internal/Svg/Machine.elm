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
import Techdraw.Event exposing (EventHandler)
import Techdraw.Internal.Dwg exposing (Dwg(..))
import Techdraw.Internal.Svg.Defs as Defs
import Techdraw.Internal.Svg.Env as Env exposing (Env, Warning)
import Techdraw.Internal.Svg.Event exposing (eventHandlersToSvgAttrs)
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
    = KontAtop1 (Dwg msg) (Env msg)
    | KontAtop2 (List (Svg msg)) (Env msg)
    | KontBeneath1 (Dwg msg) (Env msg)
    | KontBeneath2 (List (Svg msg)) (Env msg)


{-| State in the state machine.
-}
type alias State msg =
    { expr : Expr msg
    , envr : Env msg
    , kont : List (Kont msg)
    }


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

        {- Update the local-to-world transformation in the environment. -}
        ( Init (DwgTransformed childToParent drawing), _ ) ->
            state
                |> setExpr (Init drawing)
                |> modEnvr (Env.concatTransform childToParent)

        {- Draw the first drawing on top of the second. -}
        ( Init (DwgAtop topDrawing bottomDrawing), _ ) ->
            state
                |> setExpr (Init topDrawing)
                |> suspendKont (KontAtop1 bottomDrawing)

        {- Draw the first drawing beneath the second. -}
        ( Init (DwgBeneath bottomDrawing topDrawing), _ ) ->
            state
                |> setExpr (Init bottomDrawing)
                |> suspendKont (KontBeneath1 topDrawing)

        {- Add a pending event handler to the environment. -}
        ( Init (DwgEventHandler eventHandler drawing), _ ) ->
            state
                |> setExpr (Init drawing)
                |> modEnvr (Env.addEventHandler eventHandler)

        {- ----------------------------------------------------------------- -}
        {- Continuation states -}
        {- ----------------------------------------------------------------- -}
        {- Process the first `DwgAtop` continuation.

           At this continuation, we have evaluated the first argument of a
           `DwgAtop` value, and have to evaluate the second argument.
        -}
        ( Fine topSvgs, (KontAtop1 bottomDrawing susEnv) :: _ ) ->
            state
                |> threadEnvr susEnv
                |> setExpr (Init bottomDrawing)
                |> popKont
                |> suspendKont (KontAtop2 topSvgs)

        {- Process the second `DwgAtop` continuation.

           At this continuation, we have evaluated both arguments of a
           `DwgAtop` value, and have to package them together.
        -}
        ( Fine bottomSvgs, (KontAtop2 topSvgs susEnv) :: _ ) ->
            state
                |> threadEnvr susEnv
                |> setExpr (Fine <| combineDrawings susEnv topSvgs bottomSvgs)
                |> popKont

        {- Process the first `DwgBeneath` continuation.

           At this continuation, we have evaluated the first argument of a
           `DwgBeneath` value, and have to evaluate the second argument.
        -}
        ( Fine bottomSvgs, (KontBeneath1 topDrawing susEnv) :: _ ) ->
            state
                |> threadEnvr susEnv
                |> setExpr (Init topDrawing)
                |> popKont
                |> suspendKont (KontBeneath2 bottomSvgs)

        {- Process the second `DwgBeneath` continuation.

           At this continuation, we have evaluated both arguments of a
           `DwgBeneath` value, and have to package them together.
        -}
        ( Fine topSvgs, (KontBeneath2 bottomSvgs susEnv) :: _ ) ->
            state
                |> threadEnvr susEnv
                |> setExpr (Fine <| combineDrawings susEnv topSvgs bottomSvgs)
                |> popKont


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


{-| Thread the environment from completing a continuation through the
suspended environment from before the continuation was started.
-}
threadEnvr : Env msg -> State msg -> State msg
threadEnvr suspendedEnvironment oldState =
    { oldState | envr = Env.thread suspendedEnvironment oldState.envr }


{-| Create a suspended continuation with the current environment, and prepend
it to the list of pending continuations in the state.
-}
suspendKont : (Env msg -> Kont msg) -> State msg -> State msg
suspendKont envToKont oldState =
    { oldState | kont = envToKont oldState.envr :: oldState.kont }


{-| Pop the top continuation from the state.
-}
popKont : State msg -> State msg
popKont oldState =
    case oldState.kont of
        [] ->
            oldState

        _ :: ks ->
            { oldState | kont = ks }


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
    attachPendingEvents env [ TypedSvg.path attrs [] ]


{-| Combine drawings in the specified order.
-}
combineDrawings : Env msg -> List (Svg msg) -> List (Svg msg) -> List (Svg msg)
combineDrawings env topSvgs bottomSvgs =
    bottomSvgs ++ topSvgs |> attachPendingEvents env


{-| Attach any pending events from the environment to produced SVG.
-}
attachPendingEvents : Env msg -> List (Svg msg) -> List (Svg msg)
attachPendingEvents env svgs =
    let
        handlers =
            Env.getEventHandlers env
    in
    if List.isEmpty handlers then
        svgs

    else
        [ TypedSvg.g (eventHandlersToSvgAttrs env handlers) svgs ]
