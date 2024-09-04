module Techdraw.Internal.Dwg exposing (Dwg(..))

{-| Internal sum type representing drawings.

@docs Dwg

-}

import Techdraw.Internal.StyleAtom exposing (StyleAtom)
import Techdraw.Path exposing (Path)


{-| Internal drawing type.
-}
type Dwg msg
    = DwgEmpty
    | DwgPath Path
    | DwgStyled StyleAtom (Dwg msg)
