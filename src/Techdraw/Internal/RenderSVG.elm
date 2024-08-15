module Techdraw.Internal.RenderSVG exposing (render)

import Color as C
import Html exposing (Attribute)
import Html.Events as E
import Json.Decode as JD
import Techdraw.Internal.BiTransform as BT
import Techdraw.Internal.PrimTree as PT
import TypedSvg as S
import TypedSvg.Attributes as A
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as ST exposing (px)


render : PT.Drawing msg -> Svg msg
render drawing =
    let
        (ComputedSVGSize cds) =
            computeDrawingSize drawing

        (PT.Drawing dwg) =
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


renderPrim : BT.BiTransform -> DefinedStyle -> PT.PrimTree msg -> List (Svg msg)
renderPrim localToWorld dstyle primtree =
    case primtree of
        PT.PrimShape shape ->
            renderShape localToWorld dstyle shape

        PT.PrimGroup group ->
            renderGroup localToWorld dstyle group


renderShape : BT.BiTransform -> DefinedStyle -> PT.Shape msg -> List (Svg msg)
renderShape clientToLocal dstyle (PT.Shape shape) =
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
renderGroup : BT.BiTransform -> DefinedStyle -> PT.Group msg -> List (Svg msg)
renderGroup clientToLocal dstyle (PT.Group group) =
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


toSvgEventHandlers : BT.BiTransform -> PT.Events msg -> List (Attribute msg)
toSvgEventHandlers clientToLocal (PT.Events events) =
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


toSvgEventHandler : String -> BT.BiTransform -> PT.Handler msg -> Maybe (Attribute msg)
toSvgEventHandler name clientToLocal handler =
    case handler of
        PT.NoHandler ->
            Nothing

        PT.NotifyHandler message ->
            Just (E.on name (JD.succeed message))

        PT.MouseEventHandler mkMessage ->
            let
                decoder : JD.Decoder msg
                decoder =
                    JD.map2
                        (\clientX clientY ->
                            let
                                clientPt =
                                    PT.Pt { x = clientX, y = clientY }

                                mouseEvent =
                                    PT.MouseEvent
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
        { color : PT.Color
        , width : Float
        }


type DefinedFill
    = DefinedFill
        { color : PT.Color
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


toSvgPaint : PT.Color -> ST.Paint
toSvgPaint color =
    ST.Paint (toSvgColor color)


toSvgColor : PT.Color -> C.Color
toSvgColor (PT.RGBA r g b a) =
    C.fromRgba { red = r, green = g, blue = b, alpha = a }



---- Style Merging -----------------------------------------------------------


mergeStyle : PT.Style -> DefinedStyle -> DefinedStyle
mergeStyle (PT.Style style) (DefinedStyle dstyle) =
    DefinedStyle
        { stroke = mergeStroke style.stroke dstyle.stroke
        , fill = mergeFill style.fill dstyle.fill
        }


mergeStroke : PT.Stroke -> DefinedStroke -> DefinedStroke
mergeStroke (PT.Stroke stroke) (DefinedStroke dstroke) =
    DefinedStroke
        { color = Maybe.withDefault dstroke.color stroke.color
        , width = Maybe.withDefault dstroke.width stroke.width
        }


mergeFill : PT.Fill -> DefinedFill -> DefinedFill
mergeFill (PT.Fill fill) (DefinedFill dfill) =
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
        { color = PT.RGBA 0 0 0 1
        , width = 1.0
        }


initFill : DefinedFill
initFill =
    DefinedFill
        { color = PT.RGBA 0 0 0 1
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


computeDrawingSize : PT.Drawing msg -> ComputedSVGSize
computeDrawingSize (PT.Drawing dwg) =
    let
        (PT.DrawingSizeFixed { width, height }) =
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


transformPathToSvg : BT.BiTransform -> PT.Path -> Attribute msg
transformPathToSvg transform path =
    pathToSvg (transformPath transform path)


pathToSvg : PT.Path -> Attribute msg
pathToSvg path =
    A.d (toSvgPath path)


toSvgPath : PT.Path -> String
toSvgPath (PT.Path subpaths) =
    String.join " " (List.map toSvgSubPath subpaths)


toSvgSubPath : PT.SubPath -> String
toSvgSubPath (PT.SubPath moveTo commands) =
    let
        strCommands =
            List.map toSvgCommand commands
    in
    toSvgMoveTo moveTo ++ " " ++ String.join " " strCommands


toSvgCommand : PT.Command -> String
toSvgCommand command =
    case command of
        PT.CommandLine lineTo ->
            toSvgLineTo lineTo

        PT.CommandQuadraticBezier qb ->
            toSvgQuadraticBezierTo qb

        PT.CommandCubicBezier cb ->
            toSvgCubicBezierTo cb


toSvgMoveTo : PT.MoveTo -> String
toSvgMoveTo (PT.MoveTo cpt) =
    "M " ++ toSvgCpt cpt


toSvgLineTo : PT.LineTo -> String
toSvgLineTo (PT.LineTo cpt) =
    "L " ++ toSvgCpt cpt


toSvgQuadraticBezierTo : PT.QuadraticBezierTo -> String
toSvgQuadraticBezierTo (PT.QuadraticBezierTo cpt1 cpt2) =
    "Q " ++ toSvgCpt cpt1 ++ " " ++ toSvgCpt cpt2


toSvgCubicBezierTo : PT.CubicBezierTo -> String
toSvgCubicBezierTo (PT.CubicBezierTo cpt1 cpt2 cpt3) =
    "C " ++ toSvgCpt cpt1 ++ " " ++ toSvgCpt cpt2 ++ " " ++ toSvgCpt cpt3


toSvgCpt : PT.Cpt -> String
toSvgCpt (PT.Cpt cpt) =
    String.fromFloat cpt.x ++ " " ++ String.fromFloat cpt.y


transformPath : BT.BiTransform -> PT.Path -> PT.Path
transformPath transform (PT.Path subPaths) =
    PT.Path (List.map (transformSubPath transform) subPaths)


transformSubPath : BT.BiTransform -> PT.SubPath -> PT.SubPath
transformSubPath transform (PT.SubPath moveTo commands) =
    PT.SubPath
        (transformMoveTo transform moveTo)
        (List.map (transformCommand transform) commands)


transformCommand : BT.BiTransform -> PT.Command -> PT.Command
transformCommand transform command =
    case command of
        PT.CommandLine lineTo ->
            PT.CommandLine <| transformLineTo transform lineTo

        PT.CommandQuadraticBezier qbTo ->
            PT.CommandQuadraticBezier <|
                transformQuadraticBezierTo transform qbTo

        PT.CommandCubicBezier cbTo ->
            PT.CommandCubicBezier <|
                transformCubicBezierTo transform cbTo


transformMoveTo : BT.BiTransform -> PT.MoveTo -> PT.MoveTo
transformMoveTo transform (PT.MoveTo cpt) =
    PT.MoveTo (transformCpt transform cpt)


transformLineTo : BT.BiTransform -> PT.LineTo -> PT.LineTo
transformLineTo transform (PT.LineTo cpt) =
    PT.LineTo (transformCpt transform cpt)


transformQuadraticBezierTo :
    BT.BiTransform
    -> PT.QuadraticBezierTo
    -> PT.QuadraticBezierTo
transformQuadraticBezierTo transform (PT.QuadraticBezierTo cpt1 cpt2) =
    let
        xpt =
            transformCpt transform
    in
    PT.QuadraticBezierTo (xpt cpt1) (xpt cpt2)


transformCubicBezierTo :
    BT.BiTransform
    -> PT.CubicBezierTo
    -> PT.CubicBezierTo
transformCubicBezierTo transform (PT.CubicBezierTo cpt1 cpt2 cpt3) =
    let
        xpt =
            transformCpt transform
    in
    PT.CubicBezierTo (xpt cpt1) (xpt cpt2) (xpt cpt3)


transformCpt : BT.BiTransform -> PT.Cpt -> PT.Cpt
transformCpt transform (PT.Cpt pt) =
    let
        ( x, y ) =
            BT.applyFwd transform ( pt.x, pt.y )
    in
    PT.Cpt { x = x, y = y }
