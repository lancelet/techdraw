module TechdrawNew exposing
    ( Drawing
    , Sizes
    , toSvg
    , empty, path, tagCSys, styled, transformed
    , map
    )

{-| Techdraw (new API).

@docs Drawing
@docs Sizes
@docs toSvg
@docs empty, path, tagCSys, styled, transformed
@docs map

-}

import Html exposing (Html)
import Techdraw.CSys exposing (CSysName)
import Techdraw.Internal.Drawing as ID exposing (IDrawing(..))
import Techdraw.Internal.SvgDrawing as SD
import Techdraw.Math exposing (AffineTransform)
import Techdraw.Path exposing (Path)
import Techdraw.Style exposing (Style)


{-| Drawing type.
-}
type Drawing msg
    = Drawing (IDrawing msg)


{-| Sizes of a drawing.
-}
type alias Sizes =
    { containerWidth : Int
    , containerHeight : Int
    , viewBoxMinX : Float
    , viewBoxMinY : Float
    , viewBoxWidth : Float
    , viewBoxHeight : Float
    }


{-| Convert a drawing to an SVG element in HTML.
-}
toSvg : Sizes -> Drawing msg -> Html msg
toSvg sz (Drawing iDrawing) =
    SD.toSvg
        (SD.Sizes
            { containerSize =
                SD.ContainerSize
                    { width = sz.containerHeight
                    , height = sz.containerHeight
                    }
            , viewBox =
                SD.ViewBox
                    { minX = sz.viewBoxMinX
                    , minY = sz.viewBoxMinY
                    , width = sz.viewBoxWidth
                    , height = sz.viewBoxHeight
                    }
            }
        )
        iDrawing


{-| Empty drawing.
-}
empty : Drawing msg
empty =
    Drawing <| IDrawingEmpty


{-| Create a drawing from a `Path`.
-}
path : Path -> Drawing msg
path =
    Drawing << IDrawingPath


{-| Tag a coordinate system.
-}
tagCSys : CSysName -> Drawing msg
tagCSys =
    Drawing << IDrawingTagCSys


{-| Apply a style to a drawing.
-}
styled : Style -> Drawing msg -> Drawing msg
styled sty (Drawing iDrawing) =
    Drawing <| IDrawingStyled sty iDrawing


{-| Apply an affine transformation to a drawing.
-}
transformed : AffineTransform -> Drawing msg -> Drawing msg
transformed xform (Drawing iDrawing) =
    Drawing <| IDrawingTransformed xform iDrawing


{-| Map a function across the event type of the drawing.
-}
map : (a -> b) -> Drawing a -> Drawing b
map f (Drawing iDrawing) =
    Drawing <| ID.map f iDrawing
