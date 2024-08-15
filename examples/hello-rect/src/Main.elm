module Main exposing (..)

{-| Draw a rectangle!

All this is to draw a rectangle.

TODO: Improve the API.

-}

import Browser
import Html exposing (..)
import Techdraw.Internal.BiTransform as BT
import Techdraw.Internal.Drawing exposing (..)
import Techdraw.Internal.RenderSvg exposing (render)


type Model
    = Model
        { color : Color
        }


type Msg
    = MouseEnter
    | MouseExit


init : Model
init =
    Model { color = RGBA 0 0 0 1 }


path =
    Path
        [ SubPath (MoveTo (Cpt { x = 20, y = 20 }))
            [ CommandLine (LineTo (Cpt { x = 20, y = 80 }))
            , CommandLine (LineTo (Cpt { x = 80, y = 80 }))
            , CommandLine (LineTo (Cpt { x = 80, y = 20 }))
            , CommandClose
            ]
        ]


events =
    Events
        { onClick = NoHandler
        , onDoubleClick = NoHandler
        , onMouseDown = NoHandler
        , onMouseUp = NoHandler
        , onMouseEnter = NotifyHandler MouseEnter
        , onMouseLeave = NotifyHandler MouseExit
        , onMouseOver = NoHandler
        , onMouseOut = NoHandler
        }


mkFill : Color -> Fill
mkFill color =
    Fill { color = Just color }


stroke =
    Stroke { color = Nothing, width = Nothing }


mkStyle : Color -> Style
mkStyle color =
    Style { stroke = stroke, fill = mkFill color }


mkPrim : Color -> Prim Msg
mkPrim color =
    PrimShape <|
        Shape
            { events = events
            , style = mkStyle color
            , path = path
            }


drawingSize =
    DrawingSizeFixed
        { width = 100
        , height = 100
        }


mkDrawing : Color -> Drawing Msg
mkDrawing color =
    Drawing
        { size = drawingSize
        , transform = BT.identity
        , prim = mkPrim color
        }


view : Model -> Html Msg
view (Model model) =
    render (mkDrawing model.color)


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        MouseEnter ->
            Model { model | color = RGBA 1 0 0 1 }

        MouseExit ->
            Model { model | color = RGBA 0 0 0 1 }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
