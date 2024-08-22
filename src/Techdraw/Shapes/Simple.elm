module Techdraw.Shapes.Simple exposing
    ( rect, rectRounded
    , circle, ellipse
    )

{-| Simple shapes.

@docs rect, rectRounded
@docs circle, ellipse

-}

import Techdraw.Math exposing (Path(..))
import Techdraw.PathBuilder exposing (arcTo, close, createPath, empty, lineTo, moveTo)


{-| A rectangle path.
-}
rect :
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }
    -> Path
rect r =
    if r.width <= 0 || r.height <= 0 then
        EmptyPath

    else
        let
            x0 =
                r.x

            y0 =
                r.y

            x1 =
                r.x + r.width

            y1 =
                r.y + r.height
        in
        empty
            |> moveTo ( x0, y0 )
            |> lineTo ( x1, y0 )
            |> lineTo ( x1, y1 )
            |> lineTo ( x0, y1 )
            |> close
            |> createPath


{-| A rounded rectangle path.
-}
rectRounded :
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , rx : Float
    , ry : Float
    }
    -> Path
rectRounded r =
    if r.width <= 0 || r.height <= 0 then
        EmptyPath

    else
        let
            rx =
                max 0 r.rx

            ry =
                max 0 r.ry

            xa =
                r.x

            xb =
                r.x + rx

            xc =
                r.x + r.width - rx

            xd =
                r.x + r.width

            ya =
                r.y

            yb =
                r.y + ry

            yc =
                r.y + r.height - ry

            yd =
                r.y + r.height

            arc =
                { rx = rx
                , ry = ry
                , xAxisAngleDegrees = 0
                , large = False
                , sweep = False
                }
        in
        empty
            |> moveTo ( xb, ya )
            |> lineTo ( xc, ya )
            |> arcTo arc ( xd, yb )
            |> lineTo ( xd, yc )
            |> arcTo arc ( xc, yd )
            |> lineTo ( xb, yd )
            |> arcTo arc ( xa, yc )
            |> lineTo ( xa, yb )
            |> arcTo arc ( xb, ya )
            |> close
            |> createPath


{-| A circle path.
-}
circle :
    { r : Float
    , cx : Float
    , cy : Float
    }
    -> Path
circle r =
    if r.r <= 0 then
        EmptyPath

    else
        let
            arc =
                { rx = r.r
                , ry = r.r
                , xAxisAngleDegrees = 0
                , large = False
                , sweep = False
                }
        in
        empty
            |> moveTo ( r.cx + r.r, r.cy )
            |> arcTo arc ( r.cx, r.cy + r.r )
            |> arcTo arc ( r.cx - r.r, r.cy )
            |> arcTo arc ( r.cx, r.cy - r.r )
            |> arcTo arc ( r.cx + r.r, r.cy )
            |> close
            |> createPath


{-| An ellipse path.
-}
ellipse :
    { rx : Float
    , ry : Float
    , cx : Float
    , cy : Float
    }
    -> Path
ellipse r =
    if r.rx <= 0 || r.ry <= 0 then
        EmptyPath

    else
        let
            arc =
                { rx = r.rx
                , ry = r.ry
                , xAxisAngleDegrees = 0
                , large = False
                , sweep = False
                }
        in
        empty
            |> moveTo ( r.cx + r.rx, r.cy )
            |> arcTo arc ( r.cx, r.cy + r.ry )
            |> arcTo arc ( r.cx - r.rx, r.cy )
            |> arcTo arc ( r.cx, r.cy - r.ry )
            |> arcTo arc ( r.cx + r.rx, r.cy )
            |> close
            |> createPath
