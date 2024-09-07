module Depict.Internal.Dwg exposing (Dwg(..))

{-| Internal sum type representing drawings.

@docs Dwg

-}

import Depict.Event exposing (EventHandler)
import Depict.Internal.StyleAtom exposing (StyleAtom)
import Depict.Math exposing (AffineTransform)
import Depict.Path exposing (Path)
import Depict.Types exposing (CSysName, FrozenName, Visibility)


{-| Internal drawing type.
-}
type Dwg msg
    = DwgEmpty
    | DwgPath Path
    | DwgStyled StyleAtom (Dwg msg)
    | DwgTransformed AffineTransform (Dwg msg)
    | DwgAtop (Dwg msg) (Dwg msg)
    | DwgBeneath (Dwg msg) (Dwg msg)
    | DwgEventHandler (EventHandler msg) (Dwg msg)
    | DwgHostEventHandler (EventHandler msg) (Dwg msg)
    | DwgTagCSys CSysName (Dwg msg)
    | DwgFrozen Visibility (Maybe FrozenName) (Dwg msg)
    | DwgUse FrozenName
