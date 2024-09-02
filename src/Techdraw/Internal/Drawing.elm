module Techdraw.Internal.Drawing exposing
    ( IDrawing(..)
    , map
    )

{-| Internal drawing type.

The internal drawing type is the same as the external one, but we wrap it for
the external API so that it has a nicer user experience.

@docs IDrawing
@docs map

-}

import Techdraw.CSys exposing (CSysName)
import Techdraw.Datum exposing (DatumName, DatumPrefix(..))
import Techdraw.Decorator exposing (Decorator(..))
import Techdraw.Event as Event exposing (EventHandler(..))
import Techdraw.Math exposing (AffineTransform, P2)
import Techdraw.Path exposing (Path)
import Techdraw.Style exposing (Style)


{-| Internal Drawing type.
-}
type IDrawing msg
    = IDrawingEmpty
    | IDrawingPath Path
    | IDrawingTagCSys CSysName
    | IDrawingBake (IDrawing msg)
    | IDrawingStyled Style (IDrawing msg)
    | IDrawingTransformed AffineTransform (IDrawing msg)
    | IDrawingDecorated Decorator (IDrawing msg)
    | IDrawingHostEventHandler (EventHandler msg) (IDrawing msg)
    | IDrawingEventHandler (EventHandler msg) (IDrawing msg)
    | IDrawingAtop (IDrawing msg) (IDrawing msg) -- below above
    | IDrawingBeneath (IDrawing msg) (IDrawing msg) -- above below
    | IDrawingDatum DatumName P2
    | IDrawingDatumPrefix DatumPrefix (IDrawing msg)
    | IDrawingWithDatumAccess ((DatumName -> P2) -> IDrawing msg)



---- Non-Recursive Map --------------------------------------------------------


{-| Map a function over the event type of a drawing.

This is implemented using a non-recursive, continuation-based machine, so that
stack overflows are unlikely to occur due to deep drawing trees.

-}
map : (a -> b) -> IDrawing a -> IDrawing b
map f iDrawing =
    let
        -- Loop should be tail-recursive.
        loop : State a b -> IDrawing b
        loop state =
            case finished state of
                Just drawing ->
                    drawing

                Nothing ->
                    let
                        newState =
                            step state
                    in
                    loop newState
    in
    inject f iDrawing |> loop


{-| State for the non-recursive map operation.

The state consists of:

  - `mapf`: the function used for mapping
  - `expr`: the current expression
  - `kont`: the stack of pending continuations

-}
type State a b
    = State
        { mapf : a -> b
        , expr : Expr a b
        , kont : List (Kont a b)
        }


{-| Expression for the non-recursive map.

An expression can be:

  - `Final`: A completed drawing, with the final message type, `b`
  - `Initial`: An initial drawing, with the initial message type, `a`

-}
type Expr a b
    = Final (IDrawing b)
    | Initial (IDrawing a)


{-| Suspended continuations for the non-recursive map.
-}
type Kont a b
    = KontBake
    | KontStyled Style
    | KontTransformed AffineTransform
    | KontDecorated Decorator
    | KontHostEventHandler (EventHandler b)
    | KontEventHandler (EventHandler b)
    | KontAtop1 (IDrawing a)
    | KontAtop2 (IDrawing b)
    | KontBeneath1 (IDrawing a)
    | KontBeneath2 (IDrawing b)
    | KontDatumPrefix DatumPrefix


{-| Inject an initial drawing into the state.
-}
inject : (a -> b) -> IDrawing a -> State a b
inject mapf drawing =
    State { mapf = mapf, expr = Initial drawing, kont = [] }


{-| Step of the mapping operation.

The basic internal functionality of `step` is very simple. There are two main
cases:

1.  If an `Initial` drawing type is encountered, push any required
    continuation onto the stack and start evaluating a sub-component of
    the `Initial` expression into a `Final` expression.
2.  If a `Final` expression is encountered, combine it with the next
    pending continuation.

Exhaustiveness checking on the types in the `case` expression is a very
powerful check to make sure we're handing everything here.

-}
step : State a b -> State a b
step (State state) =
    case ( state.expr, state.kont ) of
        {- Transform Initial States; introduce continuations -}
        ( Initial IDrawingEmpty, _ ) ->
            State { state | expr = Final <| IDrawingEmpty }

        ( Initial (IDrawingPath path), _ ) ->
            State { state | expr = Final <| IDrawingPath path }

        ( Initial (IDrawingTagCSys csysName), _ ) ->
            State { state | expr = Final <| IDrawingTagCSys csysName }

        ( Initial (IDrawingDatum datumName point), _ ) ->
            State { state | expr = Final <| IDrawingDatum datumName point }

        ( Initial (IDrawingWithDatumAccess f), _ ) ->
            State
                { state
                    | expr =
                        Final <|
                            IDrawingWithDatumAccess <|
                                \access ->
                                    f access |> map state.mapf
                }

        ( Initial (IDrawingBake drawing), ks ) ->
            State
                { state
                    | expr = Initial drawing
                    , kont = KontBake :: ks
                }

        ( Initial (IDrawingStyled style drawing), ks ) ->
            State
                { state
                    | expr = Initial drawing
                    , kont = KontStyled style :: ks
                }

        ( Initial (IDrawingTransformed transform drawing), ks ) ->
            State
                { state
                    | expr = Initial drawing
                    , kont = KontTransformed transform :: ks
                }

        ( Initial (IDrawingDecorated decoration drawing), ks ) ->
            State
                { state
                    | expr = Initial drawing
                    , kont = KontDecorated decoration :: ks
                }

        ( Initial (IDrawingHostEventHandler handler drawing), ks ) ->
            State
                { state
                    | expr = Initial drawing
                    , kont =
                        KontHostEventHandler
                            (Event.map state.mapf handler)
                            :: ks
                }

        ( Initial (IDrawingEventHandler handler drawing), ks ) ->
            State
                { state
                    | expr = Initial drawing
                    , kont =
                        KontEventHandler
                            (Event.map state.mapf handler)
                            :: ks
                }

        ( Initial (IDrawingAtop x y), ks ) ->
            State
                { state
                    | expr = Initial x
                    , kont = KontAtop1 y :: ks
                }

        ( Initial (IDrawingBeneath x y), ks ) ->
            State
                { state
                    | expr = Initial x
                    , kont = KontBeneath1 y :: ks
                }

        ( Initial (IDrawingDatumPrefix prefix drawing), ks ) ->
            State
                { state
                    | expr = Initial drawing
                    , kont = KontDatumPrefix prefix :: ks
                }

        {- Complete continuations -}
        ( Final _, [] ) ->
            State state

        ( Final drawing, KontBake :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingBake drawing
                }

        ( Final drawing, (KontStyled style) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingStyled style drawing
                }

        ( Final drawing, (KontTransformed transform) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingTransformed transform drawing
                }

        ( Final drawing, (KontDecorated decoration) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingDecorated decoration drawing
                }

        ( Final drawing, (KontHostEventHandler handler) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingHostEventHandler handler drawing
                }

        ( Final drawing, (KontEventHandler handler) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingEventHandler handler drawing
                }

        ( Final drawing, (KontAtop1 y) :: ks ) ->
            State
                { state
                    | kont = KontAtop2 drawing :: ks
                    , expr = Initial y
                }

        ( Final drawing, (KontAtop2 x) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingAtop x drawing
                }

        ( Final drawing, (KontBeneath1 y) :: ks ) ->
            State
                { state
                    | kont = KontAtop2 drawing :: ks
                    , expr = Initial y
                }

        ( Final drawing, (KontBeneath2 x) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingBeneath x drawing
                }

        ( Final drawing, (KontDatumPrefix datumPrefix) :: ks ) ->
            State
                { state
                    | kont = ks
                    , expr = Final <| IDrawingDatumPrefix datumPrefix drawing
                }


{-| Return the drawing if mapping has finished.

A completed state has a `Final` drawing and an empty continuation stack.

-}
finished : State a b -> Maybe (IDrawing b)
finished (State state) =
    case ( state.expr, state.kont ) of
        ( Final drawing, [] ) ->
            Just drawing

        _ ->
            Nothing
