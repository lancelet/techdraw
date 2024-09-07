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
import Techdraw.Internal.Svg.Defs as Defs
import Techdraw.Internal.Svg.Env as Env exposing (Env, Warning)
import Techdraw.Internal.Svg.Event exposing (eventHandlersToSvgAttrs)
import Techdraw.Internal.Svg.Extras as Extras
import Techdraw.Internal.Svg.Path as SvgPath
import Techdraw.Internal.Svg.Style as SvgStyle
import Techdraw.Math as Math
import Techdraw.Path as Path exposing (Path)
import Techdraw.Types as T
    exposing
        ( ContainerSize
        , FrozenName(..)
        , Sizing
        , ViewBox
        , Visibility(..)
        )
import TypedSvg
import TypedSvg.Attributes as SvgA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as SvgTypes



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
produceHtml sizing (StateMachineResult result) =
    ToSvgResult
        (TypedSvg.svg
            (sizingToSvg sizing ++ htmlEventHandlers result.envr)
            result.childSvgs
        )
        (Env.getWarnings result.envr)


{-| Produce HTML event handlers from the host event handlers list.
-}
htmlEventHandlers : Env msg -> List (Attribute msg)
htmlEventHandlers env =
    eventHandlersToSvgAttrs
        (Env.getCSysDict env)
        (Env.getHostEventHandlers env)


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
    = StateMachineResult
        { childSvgs : List (Svg msg)
        , envr : Env msg
        }


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
    | KontFrozen Visibility (Maybe FrozenName) (Env msg)


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
                StateMachineResult
                    { childSvgs =
                        (Env.getDefs state.envr |> Defs.toSvg) :: svgs
                    , envr = state.envr
                    }

        _ ->
            Nothing


{-| Step the state machine.
-}
step : State msg -> State msg
step state =
    case ( state.expr, state.kont ) of
        {- ----------------------------------------------------------------- -}
        {- Terminal state

           Really we will never execute this, because `extractIfDone` will
           identify the terminal state first. But this is here to keep the
           type-checker happy and everyone sane.
        -}
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
                ( styleAttrs, state1 ) =
                    stateToStyleAttrs state
            in
            state1
                |> setExpr (Fine (pathToSvg state.envr styleAttrs path))
                |> modEnvr Env.removeEventHandlers

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
                -- Suspend the bottom drawing in a coninuation, keeping the
                -- event handlers there.
                |> suspendKont (KontAtop1 bottomDrawing)
                -- Remove the event handlers and process the top drawing.
                |> modEnvr Env.removeEventHandlers
                |> setExpr (Init topDrawing)

        {- Draw the first drawing beneath the second. -}
        ( Init (DwgBeneath bottomDrawing topDrawing), _ ) ->
            state
                -- Suspend the top drawing in a continuation, keeping the
                -- event handlers there.
                |> suspendKont (KontBeneath1 topDrawing)
                -- Remove the event handlers and process the bottom drawing.
                |> modEnvr Env.removeEventHandlers
                |> setExpr (Init bottomDrawing)

        {- Add a pending event handler to the environment. -}
        ( Init (DwgEventHandler eventHandler drawing), _ ) ->
            state
                |> setExpr (Init drawing)
                |> modEnvr (Env.addEventHandler eventHandler)

        {- Add a host event handler to the environment. -}
        ( Init (DwgHostEventHandler eventHandler drawing), _ ) ->
            state
                |> setExpr (Init drawing)
                |> modEnvr (Env.addHostEventHandler eventHandler)

        {- Tag the current local-to-world coordinate system. -}
        ( Init (DwgTagCSys cSysName drawing), _ ) ->
            state
                |> setExpr (Init drawing)
                |> modEnvr (Env.tagCSys cSysName)

        {- Freeze a drawing: convert it to SVG at this point. -}
        ( Init (DwgFrozen visibility optName drawing), _ ) ->
            state
                |> suspendKont (KontFrozen visibility optName)
                |> modEnvr Env.setLocalToWorldTransformAsInit
                |> setExpr (Init drawing)

        ( Init (DwgUse frozenName), _ ) ->
            let
                ( styleAttrs, state1 ) =
                    stateToStyleAttrs state
            in
            state1
                |> setExpr (Fine <| useToSvg state.envr styleAttrs frozenName)
                |> modEnvr Env.removeEventHandlers

        {- ----------------------------------------------------------------- -}
        {- Continuation states -}
        {- ----------------------------------------------------------------- -}
        {- Process the first `DwgAtop` continuation.

           At this continuation, we have evaluated the first argument of a
           `DwgAtop` value, and have to evaluate the second argument.
        -}
        ( Fine topSvgs, (KontAtop1 bottomDrawing susEnv) :: _ ) ->
            state
                |> popKont
                |> threadEnvr susEnv
                |> suspendKont (KontAtop2 topSvgs)
                |> modEnvr Env.removeEventHandlers
                |> setExpr (Init bottomDrawing)

        {- Process the second `DwgAtop` continuation.

           At this continuation, we have evaluated both arguments of a
           `DwgAtop` value, and have to package them together.
        -}
        ( Fine bottomSvgs, (KontAtop2 topSvgs susEnv) :: _ ) ->
            state
                |> popKont
                |> threadEnvr susEnv
                |> setExpr (Fine <| combineDrawings susEnv topSvgs bottomSvgs)

        {- Process the first `DwgBeneath` continuation.

           At this continuation, we have evaluated the first argument of a
           `DwgBeneath` value, and have to evaluate the second argument.
        -}
        ( Fine bottomSvgs, (KontBeneath1 topDrawing susEnv) :: _ ) ->
            state
                |> popKont
                |> threadEnvr susEnv
                |> suspendKont (KontBeneath2 bottomSvgs)
                |> modEnvr Env.removeEventHandlers
                |> setExpr (Init topDrawing)

        {- Process the second `DwgBeneath` continuation.

           At this continuation, we have evaluated both arguments of a
           `DwgBeneath` value, and have to package them together.
        -}
        ( Fine topSvgs, (KontBeneath2 bottomSvgs susEnv) :: _ ) ->
            state
                |> popKont
                |> threadEnvr susEnv
                |> setExpr (Fine <| combineDrawings susEnv topSvgs bottomSvgs)

        {- Process the "freeze" continuation. This attaches an optional ID
           to the frozen drawing.
        -}
        ( Fine svgs, (KontFrozen visibility optName susEnv) :: _ ) ->
            state
                |> popKont
                |> threadEnvr susEnv
                |> setExpr
                    (Fine <|
                        freezeWithOptionalId susEnv visibility optName svgs
                    )


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
stateToStyleAttrs : State msg -> ( List (Attribute msg), State msg )
stateToStyleAttrs state =
    let
        ( attrs, childEnv ) =
            SvgStyle.envStyleToSvg state.envr
    in
    ( attrs, { state | envr = Env.thread state.envr childEnv } )


{-| Convert a `Path` to SVG in the current environment.
-}
pathToSvg : Env msg -> List (Attribute msg) -> Path -> List (Svg msg)
pathToSvg env styleAttrs path =
    let
        localToWorld =
            Env.getLocalToWorld env

        numFixedDigits =
            Env.getNFixDigits env

        str =
            Path.pathApplyAffineTransform localToWorld path
                |> SvgPath.toString numFixedDigits

        attrs =
            SvgA.d str :: styleAttrs
    in
    attachPendingEvents env [ TypedSvg.path attrs [] ]


{-| Convert a "use" to SVG.
-}
useToSvg : Env msg -> List (Attribute msg) -> FrozenName -> List (Svg msg)
useToSvg env styleAttrs (FrozenName name) =
    attachPendingEvents env
        [ TypedSvg.g
            [ SvgA.transform
                -- Post-multiplying by inverse of initial transform.
                [ Extras.convertAffineTransformToSvg
                    (Math.affMatMul
                        (Env.getLocalToWorld env)
                        (Env.getInitWorldToLocal env)
                    )
                ]
            ]
            [ TypedSvg.use (SvgA.href ("#" ++ name) :: styleAttrs) []
            ]
        ]


{-| Combine drawings in the specified order.
-}
combineDrawings : Env msg -> List (Svg msg) -> List (Svg msg) -> List (Svg msg)
combineDrawings env topSvgs bottomSvgs =
    bottomSvgs ++ topSvgs |> attachPendingEvents env


{-| Attach any pending events from the environment to produced SVG.
-}
attachPendingEvents : Env msg -> List (Svg msg) -> List (Svg msg)
attachPendingEvents env svgs =
    if Env.hasPendingEventHandlers env then
        [ TypedSvg.g
            (eventHandlersToSvgAttrs
                (Env.getCSysDict env)
                (Env.getEventHandlers env)
            )
            svgs
        ]

    else
        svgs


{-| Freeze a drawing that has been converted to SVG.

  - Performs the local-to-world transformation in the group.
  - Attaches the optional frozen name as the ID.

-}
freezeWithOptionalId :
    Env msg
    -> Visibility
    -> Maybe FrozenName
    -> List (Svg msg)
    -> List (Svg msg)
freezeWithOptionalId env visibility optName svgs =
    case optName of
        Nothing ->
            svgs

        Just (FrozenName name) ->
            let
                attrs =
                    case visibility of
                        Visible ->
                            [ SvgA.transform
                                -- Post-multiplying by inverse of initial
                                -- transform.
                                [ Extras.convertAffineTransformToSvg
                                    (Math.affMatMul
                                        (Env.getLocalToWorld env)
                                        (Env.getInitWorldToLocal env)
                                    )
                                ]
                            ]

                        Hidden ->
                            [ SvgA.display SvgTypes.DisplayNone ]
            in
            [ TypedSvg.g
                attrs
                [ TypedSvg.g
                    [ SvgA.id name ]
                    svgs
                ]
            ]
