module Techdraw.Internal.Dwg exposing (Dwg(..))

{-| Internal sum type representing drawings.

@docs Dwg

-}

import Techdraw.Event exposing (EventHandler)
import Techdraw.Internal.StyleAtom exposing (StyleAtom)
import Techdraw.Math exposing (AffineTransform)
import Techdraw.Path exposing (Path)


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
