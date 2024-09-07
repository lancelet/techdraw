module Depict.Event exposing
    ( EventHandler(..)
    , MouseHandler(..), MouseInfo(..)
    , Buttons, BState(..), Modifiers, KState(..)
    , map
    )

{-| Events.

@docs EventHandler
@docs MouseHandler, MouseInfo
@docs Buttons, BState, Modifiers, KState
@docs map

-}

import Depict.Math exposing (P2)
import Depict.Types exposing (CSysName)


{-| All event handler types.
-}
type EventHandler msg
    = MouseClick (MouseHandler msg)
    | MouseContextMenu (MouseHandler msg)
    | MouseDblClick (MouseHandler msg)
    | MouseDown (MouseHandler msg)
    | MouseEnter (MouseHandler msg)
    | MouseLeave (MouseHandler msg)
    | MouseMove (MouseHandler msg)
    | MouseOut (MouseHandler msg)
    | MouseOver (MouseHandler msg)
    | MouseUp (MouseHandler msg)


{-| Mouse event handler.
-}
type MouseHandler msg
    = MouseHandler (MouseInfo -> msg)


{-| Mouse information; passed to a mouse event handler.
-}
type MouseInfo
    = MouseInfo
        { offset : P2
        , local : P2
        , buttons : Buttons
        , modifiers : Modifiers
        , pointIn : CSysName -> P2
        }


{-| State of mouse buttons.
-}
type alias Buttons =
    { b1 : BState
    , b2 : BState
    , b3 : BState
    , b4 : BState
    , b5 : BState
    }


{-| Individual mouse button state (down / up).
-}
type BState
    = BDown
    | BUp


{-| Modifier keys state.
-}
type alias Modifiers =
    { ctrl : KState
    , shift : KState
    , alt : KState
    , meta : KState
    }


{-| Individual key state (down / up).
-}
type KState
    = KDown
    | KUp


{-| Map a function over an `EventHandler` event type.
-}
map : (a -> b) -> EventHandler a -> EventHandler b
map f handler =
    case handler of
        MouseClick h ->
            mouseHandlerMap f h |> MouseClick

        MouseContextMenu h ->
            mouseHandlerMap f h |> MouseContextMenu

        MouseDblClick h ->
            mouseHandlerMap f h |> MouseDblClick

        MouseDown h ->
            mouseHandlerMap f h |> MouseDown

        MouseEnter h ->
            mouseHandlerMap f h |> MouseEnter

        MouseLeave h ->
            mouseHandlerMap f h |> MouseLeave

        MouseMove h ->
            mouseHandlerMap f h |> MouseMove

        MouseOut h ->
            mouseHandlerMap f h |> MouseOut

        MouseOver h ->
            mouseHandlerMap f h |> MouseOver

        MouseUp h ->
            mouseHandlerMap f h |> MouseUp


{-| Map a function over a `MouseHandler` event type.
-}
mouseHandlerMap : (a -> b) -> MouseHandler a -> MouseHandler b
mouseHandlerMap f (MouseHandler g) =
    MouseHandler (g >> f)
