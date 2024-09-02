module Techdraw.Internal.SvgDrawing exposing
    ( ViewBox(..), ContainerSize(..), Sizes(..)
    , toSvg
    )

{-| Converting Drawings to SVG.

@docs ViewBox, ContainerSize, Sizes
@docs toSvg

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Techdraw.CSys exposing (CSysName(..))
import Techdraw.Datum as Datum exposing (DatumName, DatumPrefix)
import Techdraw.Decorator exposing (Decorator)
import Techdraw.Event exposing (EventHandler)
import Techdraw.Internal.Drawing exposing (IDrawing(..))
import Techdraw.Internal.SvgEvent as SvgEvent
import Techdraw.Internal.SvgStringPath as SvgStringPath exposing (NFixDigits(..))
import Techdraw.Internal.SvgStyle as SvgStyle exposing (Defs, SvgStyle(..))
import Techdraw.Math as Math exposing (AffineTransform, P2)
import Techdraw.Path as Path exposing (Path)
import Techdraw.Style as Style exposing (Gradient, Style)
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttributes
import TypedSvg.Core exposing (Attribute, Svg)


{-| Sizes for a drawing.

A drawing size is defined by:

  - The size of its container, and
  - The view box which describes the drawing's initial coordinate system.

-}
type Sizes
    = Sizes
        { containerSize : ContainerSize
        , viewBox : ViewBox
        }


{-| Size for a container of a drawing, in pixels.
-}
type ContainerSize
    = ContainerSize
        { width : Int
        , height : Int
        }


{-| Viewbox defining the initial coordinate system of a drawing.
-}
type ViewBox
    = ViewBox
        { minX : Float
        , minY : Float
        , width : Float
        , height : Float
        }


{-| Convert `Sizes` to a list of attributes.
-}
sizesAttributes : Sizes -> List (Attribute msg)
sizesAttributes (Sizes sz) =
    containerSizeAttributes sz.containerSize ++ [ viewBoxAttribute sz.viewBox ]


{-| Convert `ContainerSize` to a list of attributes.
-}
containerSizeAttributes : ContainerSize -> List (Attribute msg)
containerSizeAttributes (ContainerSize sz) =
    [ HtmlAttributes.width sz.width
    , HtmlAttributes.height sz.height
    ]


{-| Convert `ViewBox` to a viewBox attribute.
-}
viewBoxAttribute : ViewBox -> Attribute msg
viewBoxAttribute (ViewBox vb) =
    SvgAttributes.viewBox vb.minX vb.minY vb.width vb.height


{-| Convert a drawing to SVG.
-}
toSvg : Sizes -> IDrawing msg -> Html msg
toSvg sizes iDrawing =
    Svg.svg
        (sizesAttributes sizes)
        (componentsToSvg sizes iDrawing)



---- State Machine for SVG Evaluation -----------------------------------------


{-| Convert drawing components to a list of SVG components.
-}
componentsToSvg : Sizes -> IDrawing msg -> List (Svg msg)
componentsToSvg sizes iDrawing =
    let
        -- Loop should be tail-recursive
        loop : State msg -> List (Svg msg)
        loop state =
            case finished state of
                Just svgs ->
                    svgs

                Nothing ->
                    let
                        newState =
                            step state
                    in
                    loop newState
    in
    inject sizes iDrawing |> loop


{-| Inject sizes and the drawing commands into an initial state.
-}
inject : Sizes -> IDrawing msg -> State msg
inject sizes iDrawing =
    State
        { expr = Initial iDrawing
        , kont = []
        , envr =
            Envr
                { nfdg = NFixDigits 2
                , tl2w = initialTransform sizes
                , styl = Style.inheritAll
                , defs = SvgStyle.emptyDefs
                , evts = []
                , hvts = []
                , decr = Nothing
                , csys = Dict.empty
                , dprf = []
                , datm = Dict.empty
                }
        }


{-| Compute the initial transformation from the sizes.
-}
initialTransform : Sizes -> AffineTransform
initialTransform (Sizes sizes) =
    let
        (ViewBox vb) =
            sizes.viewBox
    in
    Math.affFromComponents
        [ Math.AffineScaling <|
            Math.Scaling 1 -1
        , Math.AffineTranslation <|
            Math.Translation (Math.v2 vb.minX (vb.height + vb.minY))
        ]


{-| Expression in the continuation machine.

An expression can be either:

  - `Initial`: drawing expression to be converted
  - `Final`: list of produced Svg elements

-}
type Expr msg
    = Initial (IDrawing msg)
    | Final (List (Svg msg))


{-| Continuations in the continuation machine.

Continuations represent pending steps to be completed after the current
expression has reached a `Final` form. Continuations hold the environment
that existed when they were invoked.

-}
type Kont msg
    = KontAtop1 (Envr msg) (IDrawing msg)
    | KontAtop2 (Envr msg) (List (Svg msg))
    | KontBeneath1 (Envr msg) (IDrawing msg)
    | KontBeneath2 (Envr msg) (List (Svg msg))
    | KontWithDatumAccess (Envr msg)


{-| State for the evaluation machine.
-}
type State msg
    = State
        { expr : Expr msg
        , kont : List (Kont msg)
        , envr : Envr msg
        }


{-| If the state machine has finished computing, return its list of
Svg components.
-}
finished : State msg -> Maybe (List (Svg msg))
finished (State state) =
    case ( state.expr, state.kont ) of
        ( Final svgs, [] ) ->
            Just svgs

        _ ->
            Nothing


{-| Environment.

  - `nfdg`: Number of fixed digits to use when drawing path elements.
  - `tl2w`: Local-to-world affine transformation.
  - `styl`: Current style.
  - `defs`: Items to include in a `<defs>` element.
  - `evts`: Pending event handlers to be attached to a drawing.
  - `hvts`: Pending host event handlers to be attached to a host SVG element.
  - `decr`: Optional decorator (one only for now) on paths.
  - `csys`: Dictionary of coordinate system names to their local-to-world
    transformations.
  - `dprf`: List of datum name prefixes to apply to any datum points declared.
  - `datm`: Dictionary of datum points in world space.

-}
type Envr msg
    = Envr
        { nfdg : NFixDigits
        , tl2w : AffineTransform
        , styl : Style
        , defs : Defs
        , evts : List (EventHandler msg)
        , hvts : List (EventHandler msg)
        , decr : Maybe Decorator
        , csys : Dict String AffineTransform
        , dprf : List DatumPrefix
        , datm : Dict String P2
        }


step : State msg -> State msg
step (State state) =
    let
        (Envr envr) =
            state.envr
    in
    case ( state.expr, state.kont ) of
        {- Transform initial states; introduce continuations -}
        ( Initial IDrawingEmpty, _ ) ->
            State { state | expr = Final [] }

        ( Initial (IDrawingPath path), _ ) ->
            let
                ( svgPath, childEnvr ) =
                    envrPathToSvg state.envr path
            in
            State
                { state
                    | expr = Final <| [ svgPath ]
                    , envr = envrThread state.envr childEnvr
                }

        ( Initial (IDrawingTagCSys csysName), _ ) ->
            State
                { state
                    | expr = Final []
                    , envr = envrAddCSys csysName envr.tl2w state.envr
                }

        ( Initial (IDrawingBake _), _ ) ->
            Debug.todo "TODO: IDrawingBake"

        ( Initial (IDrawingStyled style drawing), _ ) ->
            State
                { state
                    | expr = Initial drawing
                    , envr = envrCombineStyle style state.envr
                }

        ( Initial (IDrawingTransformed transform drawing), _ ) ->
            State
                { state
                    | expr = Initial drawing
                    , envr = envrCombineTransform transform state.envr
                }

        ( Initial (IDrawingDecorated decorator drawing), _ ) ->
            State
                { state
                    | expr = Initial drawing
                    , envr = envrSetDecorator decorator state.envr
                }

        ( Initial (IDrawingHostEventHandler handler drawing), _ ) ->
            State
                { state
                    | expr = Initial drawing
                    , envr = envrAppendHostEventHandler handler state.envr
                }

        ( Initial (IDrawingEventHandler handler drawing), _ ) ->
            State
                { state
                    | expr = Initial drawing
                    , envr = envrAppendEventHandler handler state.envr
                }

        ( Initial (IDrawingAtop x y), ks ) ->
            State
                { state
                    | expr = Initial x
                    , kont = KontAtop1 state.envr y :: ks
                }

        ( Initial (IDrawingBeneath x y), ks ) ->
            State
                { state
                    | expr = Initial x
                    , kont = KontBeneath1 state.envr y :: ks
                }

        ( Initial (IDrawingDatum datumName point), _ ) ->
            State
                { state
                    | expr = Final []
                    , envr = envrSetDatum datumName point state.envr
                }

        ( Initial (IDrawingDatumPrefix prefix drawing), _ ) ->
            State
                { state
                    | expr = Initial drawing
                    , envr = envrPrependDatumPrefix prefix state.envr
                }

        ( Initial (IDrawingWithDatumAccess createFn), ks ) ->
            State
                { state
                    | expr = Initial <| createFn (envrDatumAccessFn state.envr)
                    , kont = KontWithDatumAccess state.envr :: ks
                }

        {- Complete continuations -}
        ( Final svgs, (KontAtop1 kenvr y) :: ks ) ->
            let
                tenvr =
                    envrThread kenvr state.envr
            in
            State
                { state
                    | expr = Initial y
                    , envr = tenvr
                    , kont = KontAtop2 tenvr svgs :: ks
                }

        ( Final svgs, (KontAtop2 kenvr xs) :: ks ) ->
            let
                tenvr =
                    envrThread kenvr state.envr
            in
            State
                { state
                    | expr = Final <| envrStack state.envr xs svgs
                    , envr = tenvr
                    , kont = ks
                }

        ( Final svgs, (KontBeneath1 kenvr y) :: ks ) ->
            let
                tenvr =
                    envrThread kenvr state.envr
            in
            State
                { state
                    | expr = Initial y
                    , envr = tenvr
                    , kont = KontBeneath2 tenvr svgs :: ks
                }

        ( Final svgs, (KontBeneath2 kenvr xs) :: ks ) ->
            let
                tenvr =
                    envrThread kenvr state.envr
            in
            State
                { state
                    | expr = Final <| envrStack state.envr svgs xs
                    , envr = tenvr
                    , kont = ks
                }

        ( Final _, (KontWithDatumAccess kenvr) :: ks ) ->
            State
                { state
                    | envr = envrThread kenvr state.envr
                    , kont = ks
                }

        {- Final, completed state. -}
        ( Final _, [] ) ->
            State state


{-| Add a named coordinate system to the environment.
-}
envrAddCSys : CSysName -> AffineTransform -> Envr msg -> Envr msg
envrAddCSys (CSysName name) l2w (Envr parent) =
    Envr { parent | csys = Dict.insert name l2w parent.csys }


{-| Combine a child style with that of its parent in the environment.
-}
envrCombineStyle : Style -> Envr msg -> Envr msg
envrCombineStyle child (Envr parent) =
    Envr { parent | styl = Style.combineStyle parent.styl child }


{-| Combine a child transformation with that of its parent in the
environment.
-}
envrCombineTransform : AffineTransform -> Envr msg -> Envr msg
envrCombineTransform child (Envr parent) =
    Envr { parent | tl2w = Math.affMatMul parent.tl2w child }


{-| Set the decorator in the current environment.
-}
envrSetDecorator : Decorator -> Envr msg -> Envr msg
envrSetDecorator decorator (Envr parent) =
    Envr { parent | decr = Just decorator }


{-| Append a host event handler to the environment.
-}
envrAppendHostEventHandler : EventHandler msg -> Envr msg -> Envr msg
envrAppendHostEventHandler handler (Envr parent) =
    Envr { parent | hvts = handler :: parent.hvts }


{-| Append a drawing event handler to the environment
-}
envrAppendEventHandler : EventHandler msg -> Envr msg -> Envr msg
envrAppendEventHandler handler (Envr parent) =
    Envr { parent | evts = handler :: parent.evts }


{-| Set a datum point in the environment as its world-space name.
-}
envrSetDatum : DatumName -> P2 -> Envr msg -> Envr msg
envrSetDatum datumName point (Envr parent) =
    Envr
        { parent
            | datm =
                Dict.insert
                    (Datum.createStringName parent.dprf datumName)
                    (Math.p2ApplyAffineTransform parent.tl2w point)
                    parent.datm
        }


{-| Prepend a datum prefix string to the environment.
-}
envrPrependDatumPrefix : DatumPrefix -> Envr msg -> Envr msg
envrPrependDatumPrefix prefix (Envr parent) =
    Envr { parent | dprf = prefix :: parent.dprf }


{-| Provide a datum access function for the environment.
-}
envrDatumAccessFn : Envr msg -> (DatumName -> P2)
envrDatumAccessFn (Envr envr) =
    let
        tw2l =
            Math.affInvert envr.tl2w
    in
    \name ->
        Dict.get (Datum.datumNameToString name) envr.datm
            |> Maybe.withDefault (Math.p2 0 0)
            |> Math.p2ApplyAffineTransform tw2l


{-| Thread an environment.

When processing has completed on a child node, it produces the `chld`
environment. This is combined with the `kont` environment from the
continuation to produce the environment that the continuation should be
processed with.

Items that should be inherited parent-to-child come from the `kont`
environment. Items that should be inherited via a depth-first evaluation
order come from the `chld` environment.

-}
envrThread : Envr msg -> Envr msg -> Envr msg
envrThread (Envr kont) (Envr chld) =
    Envr
        { nfdg = kont.nfdg
        , tl2w = kont.tl2w
        , styl = kont.styl
        , defs = chld.defs
        , evts = kont.evts
        , hvts = kont.hvts
        , decr = kont.decr
        , csys = chld.csys
        , dprf = kont.dprf
        , datm = chld.datm
        }


{-| Convert a `Path` to SVG, applying correct attributes for the
environment.
-}
envrPathToSvg : Envr msg -> Path -> ( Svg msg, Envr msg )
envrPathToSvg envr path =
    let
        (Envr env) =
            envr

        ( attrs, newEnvr ) =
            envrPathAttributes envr

        transformedPath =
            Path.pathApplyAffineTransform env.tl2w path
    in
    ( pathToSvg env.nfdg attrs transformedPath, newEnvr )


{-| Convert the style information from an `Envr` into a list of SVG
Attributes and any required child elements. Child elements are required
for gradients.

It returns a new `Envr` containing any gradients added to the internal
dictionary.

-}
envrStyleToAttributes : Envr msg -> ( List (Attribute msg), Envr msg )
envrStyleToAttributes (Envr envr) =
    let
        (SvgStyle svgStyle) =
            SvgStyle.styleToSvgAttributes envr.styl
    in
    ( svgStyle.attributes
    , Envr
        { envr
            | defs = SvgStyle.defsUnion svgStyle.defs envr.defs
        }
    )


{-| Convert the list of item events from an `Envr` into a list of SVG }
Attributes.
-}
envrEventsToAttributes : Envr msg -> List (Attribute msg)
envrEventsToAttributes (Envr envr) =
    SvgEvent.eventHandlersToSvgAttributes envr.evts


{-| Fetch all the parts from the environment that should be applied to
a `Path`.
-}
envrPathAttributes :
    Envr msg
    -> ( List (Attribute msg), Envr msg )
envrPathAttributes envr =
    let
        ( styleAttr, newEnvr ) =
            envrStyleToAttributes envr
    in
    ( styleAttr ++ envrEventsToAttributes envr, newEnvr )


{-| Convert a Path to an Svg node, with a list of attributes for styles
and events.
-}
pathToSvg :
    NFixDigits
    -> List (Attribute msg)
    -> Path
    -> Svg msg
pathToSvg nFixDigits attrs path =
    Svg.path
        (SvgAttributes.d
            (SvgStringPath.formatPath nFixDigits path
                |> SvgStringPath.svgStringPathToString
            )
            :: attrs
        )
        []


{-| Use the environment to stack nodes.

If the environment has event handlers, these are discharged directly by
creating an SVG group. However, if there are no event handlers, the two
lists of SVG are just appended.

-}
envrStack : Envr msg -> List (Svg msg) -> List (Svg msg) -> List (Svg msg)
envrStack (Envr envr) below above =
    if List.isEmpty envr.evts then
        below ++ above

    else
        [ Svg.g (envrEventsToAttributes (Envr envr)) (below ++ above) ]
