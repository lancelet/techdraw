module Techdraw.Internal.SvgEvent exposing (eventHandlersToSvgAttributes)

{-| Convert event information to SVG.

@docs eventHandlersToSvgAttributes

-}

import Techdraw.Event exposing (EventHandler)
import TypedSvg.Core exposing (Attribute)


{-| Convert a list of event handlers to a list of SVG `Attribute`s.
-}
eventHandlersToSvgAttributes : List (EventHandler msg) -> List (Attribute msg)
eventHandlersToSvgAttributes =
    Debug.todo "TODO"
