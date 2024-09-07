module Depict.Types exposing
    ( Sizing(..), ViewBox(..), ContainerSize(..)
    , sizingScaled, sizingWH
    , sizingGetViewBox, sizingGetContainerSize
    , containerSizeGetWidth, containerSizeGetHeight
    , viewBoxGetXMin, viewBoxGetYMin, viewBoxGetWidth, viewBoxGetHeight
    , Order(..), Visibility(..)
    , CSysName(..)
    , FrozenName(..)
    )

{-| Additional types.


# Sizes

@docs Sizing, ViewBox, ContainerSize
@docs sizingScaled, sizingWH
@docs sizingGetViewBox, sizingGetContainerSize
@docs containerSizeGetWidth, containerSizeGetHeight
@docs viewBoxGetXMin, viewBoxGetYMin, viewBoxGetWidth, viewBoxGetHeight


# Drawing Order

@docs Order, Visibility


# Names

@docs CSysName
@docs FrozenName

-}

---- Sizes --------------------------------------------------------------------


{-| Overall sizing of a drawing.

This indicates both the:

  - Container size: the size of the drawing container, and
  - ViewBox: the viewing window onto the drawing.

Both sizes are required to fully establish the mouse coordinate transformations.

-}
type Sizing
    = Sizing
        { containerSize : ContainerSize
        , viewBox : ViewBox
        }


{-| Viewing box for a drawing.

The `ViewBox` provides a description of the view that the user has onto the
drawing.

Initially, every drawing has the coordinate `p2 xMin yMin` in the lower-left,
with the `x` axis increasing to the right and the `y` axis increasing upwards.
The `width` and `height` of the drawing are also specified.

-}
type ViewBox
    = ViewBox
        { xMin : Float
        , yMin : Float
        , width : Float
        , height : Float
        }


{-| Size of the container the drawing will be displayed inside.

The `ContainerSize` is the size of the HTML element (an `<svg>` element) that
will contain the drawing.

-}
type ContainerSize
    = ContainerSize
        { width : Int
        , height : Int
        }


{-| Create overall drawing sizing by scaling the viewbox width and height to
produce a container size.
-}
sizingScaled :
    { xMin : Float
    , yMin : Float
    , width : Float
    , height : Float
    }
    -> Float
    -> Sizing
sizingScaled vb scale =
    Sizing
        { containerSize =
            ContainerSize
                { width = round (vb.width * scale)
                , height = round (vb.height * scale)
                }
        , viewBox =
            ViewBox
                { xMin = vb.xMin
                , yMin = vb.yMin
                , width = vb.width
                , height = vb.height
                }
        }


{-| Create `Sizing` from an unscaled width and height.

This is equivalent to placing the origin of the drawing in the lower-left and
having a 1:1 pixel-to-unit correspondence. The same width and height are used
for both the `ViewBox` and `ContainerSize`.

-}
sizingWH : Float -> Float -> Sizing
sizingWH width height =
    sizingScaled { xMin = 0, yMin = 0, width = width, height = height } 1


{-| Return the `ViewBox` from `Sizing`.
-}
sizingGetViewBox : Sizing -> ViewBox
sizingGetViewBox (Sizing sizing) =
    sizing.viewBox


{-| Return the `ContainerSize` from `Sizing`.
-}
sizingGetContainerSize : Sizing -> ContainerSize
sizingGetContainerSize (Sizing sizing) =
    sizing.containerSize


{-| Return the width of a `ContainerSize`.
-}
containerSizeGetWidth : ContainerSize -> Int
containerSizeGetWidth (ContainerSize cs) =
    cs.width


{-| Return the height of a `ContainerSize`.
-}
containerSizeGetHeight : ContainerSize -> Int
containerSizeGetHeight (ContainerSize cs) =
    cs.height


{-| Return the xMin value from a `ViewBox`.
-}
viewBoxGetXMin : ViewBox -> Float
viewBoxGetXMin (ViewBox vb) =
    vb.xMin


{-| Return the yMin value from a `ViewBox`.
-}
viewBoxGetYMin : ViewBox -> Float
viewBoxGetYMin (ViewBox vb) =
    vb.yMin


{-| Return the width value from a `ViewBox`.
-}
viewBoxGetWidth : ViewBox -> Float
viewBoxGetWidth (ViewBox vb) =
    vb.width


{-| Return the height value from a `ViewBox`.
-}
viewBoxGetHeight : ViewBox -> Float
viewBoxGetHeight (ViewBox vb) =
    vb.height



---- Drawing Order ------------------------------------------------------------


{-| When processing a list of drawings, do they appear in top-to-bottom order,
or bottom-to-top order?
-}
type Order
    = TopToBottom
    | BottomToTop



---- Visibility ---------------------------------------------------------------


{-| Visibility of an item.
-}
type Visibility
    = Visible
    | Hidden



---- Names --------------------------------------------------------------------


{-| Name of a coordinate system.
-}
type CSysName
    = CSysName String


{-| Name of a frozen drawing.

Let it go.

-}
type FrozenName
    = FrozenName String
