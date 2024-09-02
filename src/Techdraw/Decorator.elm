module Techdraw.Decorator exposing (..)

{-| Path decorators (arrow-heads, length shortening, etc.
-}

import Techdraw.Math exposing (AffineTransform)
import Techdraw.Path exposing (Path)
import Techdraw.Style exposing (Style)


{-| Decorator for a path.
-}
type Decorator
    = Decorator (RenderPath -> List RenderPath)


type RenderPath
    = RenderPath
        { style : Style
        , localToWorld : AffineTransform
        , localPath : Path
        }
