module Techdraw.Internal.Drawing exposing (..)

{-| Drawing data type.
-}

import Techdraw.Internal.BiTransform exposing (BiTransform)



---- Data Types --------------------------------------------------------------


type Drawing msg
    = Drawing
        { size : DrawingSize
        , transform : BiTransform
        , prim : Prim msg
        }


type DrawingSize
    = DrawingSizeFixed
        { width : Float
        , height : Float
        }


type Prim msg
    = PrimShape (Shape msg)
    | PrimGroup (Group msg)


type Shape msg
    = Shape
        { events : Events msg
        , style : Style
        , path : Path
        }


type Group msg
    = Group
        { events : Events msg
        , transform : BiTransform
        , style : Style
        , children : List (Prim msg)
        }


type Style
    = Style
        { stroke : Stroke
        , fill : Fill
        }


type Stroke
    = Stroke
        { color : Maybe Color
        , width : Maybe Float
        }


type Fill
    = Fill
        { color : Maybe Color
        }


type Color
    = RGBA Float Float Float Float


type Path
    = Path (List SubPath)


type SubPath
    = SubPath MoveTo (List Command)


type Command
    = CommandLine LineTo
    | CommandQuadraticBezier QuadraticBezierTo
    | CommandCubicBezier CubicBezierTo
    | CommandClose


type MoveTo
    = MoveTo Cpt


type LineTo
    = LineTo Cpt


type QuadraticBezierTo
    = QuadraticBezierTo Cpt Cpt


type CubicBezierTo
    = CubicBezierTo Cpt Cpt Cpt


type Cpt
    = Cpt
        { x : Float
        , y : Float
        }


type Events msg
    = Events
        { onClick : Handler msg
        , onDoubleClick : Handler msg
        , onMouseDown : Handler msg
        , onMouseUp : Handler msg
        , onMouseEnter : Handler msg
        , onMouseLeave : Handler msg
        , onMouseOver : Handler msg
        , onMouseOut : Handler msg
        }


type Handler msg
    = NoHandler
    | NotifyHandler msg
    | MouseEventHandler (MouseEvent -> msg)


type MouseEvent
    = MouseEvent
        { clientToLocal : BiTransform
        , client : Pt
        }


type Pt
    = Pt { x : Float, y : Float }
