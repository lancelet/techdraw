module Techdraw.Internal.RenderSvg exposing (render)

import Color as C
import Html exposing (Attribute)
import Html.Events as E
import Json.Decode as JD
import Techdraw.Internal.BiTransform as BT
import Techdraw.Internal.Drawing as D
import TypedSvg as S
import TypedSvg.Attributes as A
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as ST exposing (px)


render : D.Drawing msg -> Svg msg
render drawing =
    let
        (ComputedSVGSize cds) =
            computeDrawingSize drawing

        (D.Drawing dwg) =
            drawing
    in
    S.svg
        [ A.width (px cds.width)
        , A.height (px cds.height)
        , A.viewBox
            cds.viewBoxMinX
            cds.viewBoxMinY
            cds.viewBoxWidth
            cds.viewBoxHeight
        ]
        (renderPrim dwg.transform initStyle dwg.prim)


renderPrim : BT.BiTransform -> DefinedStyle -> D.Prim msg -> List (Svg msg)
renderPrim localToWorld dstyle primtree =
    case primtree of
        D.PrimShape shape ->
            renderShape localToWorld dstyle shape

        D.PrimGroup group ->
            renderGroup localToWorld dstyle group


renderShape : BT.BiTransform -> DefinedStyle -> D.Shape msg -> List (Svg msg)
renderShape clientToLocal dstyle (D.Shape shape) =
    let
        d : Attribute msg
        d =
            transformPathToSvg clientToLocal shape.path

        style : List (Attribute msg)
        style =
            toSvgDefinedStyle (mergeStyle shape.style dstyle)

        events : List (Attribute msg)
        events =
            toSvgEventHandlers clientToLocal shape.events

        attrs : List (Attribute msg)
        attrs =
            d :: (style ++ events)
    in
    [ S.path attrs [] ]


{-| In renderGroup, we just apply the group transform and push everything
else down.
-}
renderGroup : BT.BiTransform -> DefinedStyle -> D.Group msg -> List (Svg msg)
renderGroup clientToLocal dstyle (D.Group group) =
    let
        events : List (Attribute msg)
        events =
            toSvgEventHandlers clientToLocal group.events

        newClientToLocal : BT.BiTransform
        newClientToLocal =
            BT.compose clientToLocal group.transform

        newDStyle : DefinedStyle
        newDStyle =
            mergeStyle group.style dstyle

        children : List (Svg msg)
        children =
            List.concat <|
                List.map
                    (renderPrim newClientToLocal newDStyle)
                    group.children
    in
    [ S.g events children ]



---- Event Translation -------------------------------------------------------


toSvgEventHandlers : BT.BiTransform -> D.Events msg -> List (Attribute msg)
toSvgEventHandlers clientToLocal (D.Events events) =
    let
        mk name access =
            toSvgEventHandler name clientToLocal (access events)
    in
    List.filterMap identity <|
        [ mk "click" .onClick
        , mk "dblclick" .onDoubleClick
        , mk "mousedown" .onMouseDown
        , mk "mouseup" .onMouseUp
        , mk "mouseenter" .onMouseEnter
        , mk "mouseleave" .onMouseLeave
        , mk "mouseover" .onMouseOver
        , mk "mouseout" .onMouseOut
        ]


toSvgEventHandler : String -> BT.BiTransform -> D.Handler msg -> Maybe (Attribute msg)
toSvgEventHandler name clientToLocal handler =
    case handler of
        D.NoHandler ->
            Nothing

        D.NotifyHandler message ->
            Just (E.on name (JD.succeed message))

        D.MouseEventHandler mkMessage ->
            let
                decoder : JD.Decoder msg
                decoder =
                    JD.map2
                        (\clientX clientY ->
                            let
                                clientPt =
                                    D.Pt { x = clientX, y = clientY }

                                mouseEvent =
                                    D.MouseEvent
                                        { clientToLocal = clientToLocal
                                        , client = clientPt
                                        }
                            in
                            mkMessage mouseEvent
                        )
                        (JD.field "offsetX" JD.float)
                        (JD.field "offsetY" JD.float)
            in
            Just (E.on name decoder)



---- Fully-Defined Styles ----------------------------------------------------


type DefinedStyle
    = DefinedStyle
        { stroke : DefinedStroke
        , fill : DefinedFill
        }


type DefinedStroke
    = DefinedStroke
        { color : D.Color
        , width : Float
        }


type DefinedFill
    = DefinedFill
        { color : D.Color
        }


toSvgDefinedStyle : DefinedStyle -> List (Attribute msg)
toSvgDefinedStyle (DefinedStyle dstyle) =
    List.concat
        [ toSvgDefinedStroke dstyle.stroke
        , toSvgDefinedFill dstyle.fill
        ]


toSvgDefinedStroke : DefinedStroke -> List (Attribute msg)
toSvgDefinedStroke (DefinedStroke dstroke) =
    [ A.strokeWidth (px dstroke.width)
    , A.stroke (toSvgPaint dstroke.color)
    ]


toSvgDefinedFill : DefinedFill -> List (Attribute msg)
toSvgDefinedFill (DefinedFill dfill) =
    [ A.fill (toSvgPaint dfill.color)
    ]


toSvgPaint : D.Color -> ST.Paint
toSvgPaint color =
    ST.Paint (toSvgColor color)


toSvgColor : D.Color -> C.Color
toSvgColor (D.RGBA r g b a) =
    C.fromRgba { red = r, green = g, blue = b, alpha = a }



---- Style Merging -----------------------------------------------------------


mergeStyle : D.Style -> DefinedStyle -> DefinedStyle
mergeStyle (D.Style style) (DefinedStyle dstyle) =
    DefinedStyle
        { stroke = mergeStroke style.stroke dstyle.stroke
        , fill = mergeFill style.fill dstyle.fill
        }


mergeStroke : D.Stroke -> DefinedStroke -> DefinedStroke
mergeStroke (D.Stroke stroke) (DefinedStroke dstroke) =
    DefinedStroke
        { color = Maybe.withDefault dstroke.color stroke.color
        , width = Maybe.withDefault dstroke.width stroke.width
        }


mergeFill : D.Fill -> DefinedFill -> DefinedFill
mergeFill (D.Fill fill) (DefinedFill dfill) =
    DefinedFill
        { color = Maybe.withDefault dfill.color fill.color
        }



---- Initial Style -----------------------------------------------------------


initStyle : DefinedStyle
initStyle =
    DefinedStyle
        { stroke = initStroke
        , fill = initFill
        }


initStroke : DefinedStroke
initStroke =
    DefinedStroke
        { color = D.RGBA 0 0 0 1
        , width = 1.0
        }


initFill : DefinedFill
initFill =
    DefinedFill
        { color = D.RGBA 0 0 0 1
        }



---- Drawing size calculation ------------------------------------------------


type ComputedSVGSize
    = ComputedSVGSize
        { width : Float
        , height : Float
        , viewBoxMinX : Float
        , viewBoxMinY : Float
        , viewBoxWidth : Float
        , viewBoxHeight : Float
        }


computeDrawingSize : D.Drawing msg -> ComputedSVGSize
computeDrawingSize (D.Drawing dwg) =
    let
        (D.DrawingSizeFixed { width, height }) =
            dwg.size
    in
    ComputedSVGSize
        { width = width
        , height = height
        , viewBoxMinX = 0
        , viewBoxMinY = 0
        , viewBoxWidth = width
        , viewBoxHeight = height
        }



---- Path transformation -----------------------------------------------------


transformPathToSvg : BT.BiTransform -> D.Path -> Attribute msg
transformPathToSvg transform path =
    pathToSvg (transformPath transform path)


pathToSvg : D.Path -> Attribute msg
pathToSvg path =
    A.d (toSvgPath path)


toSvgPath : D.Path -> String
toSvgPath (D.Path subpaths) =
    String.join " " (List.map toSvgSubPath subpaths)


toSvgSubPath : D.SubPath -> String
toSvgSubPath (D.SubPath moveTo commands) =
    let
        strCommands =
            List.map toSvgCommand commands
    in
    toSvgMoveTo moveTo ++ " " ++ String.join " " strCommands


toSvgCommand : D.Command -> String
toSvgCommand command =
    case command of
        D.CommandLine lineTo ->
            toSvgLineTo lineTo

        D.CommandQuadraticBezier qb ->
            toSvgQuadraticBezierTo qb

        D.CommandCubicBezier cb ->
            toSvgCubicBezierTo cb

        D.CommandClose ->
            "Z"


toSvgMoveTo : D.MoveTo -> String
toSvgMoveTo (D.MoveTo cpt) =
    "M " ++ toSvgCpt cpt


toSvgLineTo : D.LineTo -> String
toSvgLineTo (D.LineTo cpt) =
    "L " ++ toSvgCpt cpt


toSvgQuadraticBezierTo : D.QuadraticBezierTo -> String
toSvgQuadraticBezierTo (D.QuadraticBezierTo cpt1 cpt2) =
    "Q " ++ toSvgCpt cpt1 ++ " " ++ toSvgCpt cpt2


toSvgCubicBezierTo : D.CubicBezierTo -> String
toSvgCubicBezierTo (D.CubicBezierTo cpt1 cpt2 cpt3) =
    "C " ++ toSvgCpt cpt1 ++ " " ++ toSvgCpt cpt2 ++ " " ++ toSvgCpt cpt3


toSvgCpt : D.Cpt -> String
toSvgCpt (D.Cpt cpt) =
    String.fromFloat cpt.x ++ " " ++ String.fromFloat cpt.y


transformPath : BT.BiTransform -> D.Path -> D.Path
transformPath transform (D.Path subPaths) =
    D.Path (List.map (transformSubPath transform) subPaths)


transformSubPath : BT.BiTransform -> D.SubPath -> D.SubPath
transformSubPath transform (D.SubPath moveTo commands) =
    D.SubPath
        (transformMoveTo transform moveTo)
        (List.map (transformCommand transform) commands)


transformCommand : BT.BiTransform -> D.Command -> D.Command
transformCommand transform command =
    case command of
        D.CommandLine lineTo ->
            D.CommandLine <| transformLineTo transform lineTo

        D.CommandQuadraticBezier qbTo ->
            D.CommandQuadraticBezier <|
                transformQuadraticBezierTo transform qbTo

        D.CommandCubicBezier cbTo ->
            D.CommandCubicBezier <|
                transformCubicBezierTo transform cbTo

        D.CommandClose ->
            D.CommandClose


transformMoveTo : BT.BiTransform -> D.MoveTo -> D.MoveTo
transformMoveTo transform (D.MoveTo cpt) =
    D.MoveTo (transformCpt transform cpt)


transformLineTo : BT.BiTransform -> D.LineTo -> D.LineTo
transformLineTo transform (D.LineTo cpt) =
    D.LineTo (transformCpt transform cpt)


transformQuadraticBezierTo :
    BT.BiTransform
    -> D.QuadraticBezierTo
    -> D.QuadraticBezierTo
transformQuadraticBezierTo transform (D.QuadraticBezierTo cpt1 cpt2) =
    let
        xpt =
            transformCpt transform
    in
    D.QuadraticBezierTo (xpt cpt1) (xpt cpt2)


transformCubicBezierTo :
    BT.BiTransform
    -> D.CubicBezierTo
    -> D.CubicBezierTo
transformCubicBezierTo transform (D.CubicBezierTo cpt1 cpt2 cpt3) =
    let
        xpt =
            transformCpt transform
    in
    D.CubicBezierTo (xpt cpt1) (xpt cpt2) (xpt cpt3)


transformCpt : BT.BiTransform -> D.Cpt -> D.Cpt
transformCpt transform (D.Cpt pt) =
    let
        ( x, y ) =
            BT.applyFwd transform ( pt.x, pt.y )
    in
    D.Cpt { x = x, y = y }
