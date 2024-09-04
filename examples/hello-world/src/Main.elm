module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import Techdraw as TD exposing (Drawing)
import Techdraw.Math as Math exposing (p2)
import Techdraw.Shapes.Simple exposing (circle)
import Techdraw.Style as Style exposing (LinearGradient, Stop(..))
import Techdraw.Types as TT


type Model
    = Model ()


type Msg
    = Msg ()


init : Model
init =
    Model ()


lg : LinearGradient
lg =
    Style.linearGradient
        { start = p2 (300 - 180) (200 - 180)
        , end = p2 (300 + 180) (200 + 180)
        , transform = Math.affIdentity
        , gradient =
            Style.gradient
                [ Stop 0 Color.green
                , Stop 1 Color.yellow
                ]
        }


drawing : Model -> Drawing msg
drawing _ =
    TD.path
        (circle
            { r = 180
            , cx = 300
            , cy = 200
            }
        )
        |> TD.fill (Style.PaintLinearGradient lg)
        |> TD.stroke (Style.Paint Color.black)
        |> TD.strokeWidth 5


view : Model -> Html msg
view model =
    Html.div [] [ drawing model |> TD.toSvg (TT.sizingWH 600 400) ]


update : Msg -> Model -> Model
update _ model =
    model


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
