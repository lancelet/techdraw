module Techdraw.Internal.Svg.Env exposing
    ( Env
    , Warning
    , unWarning
    , getLocalToWorld, getWarnings, getNFixDigits, getStyle
    , modStyle, applyStyleAtom
    , getDefs, modDefs
    , init
    , thread
    )

{-| Environment for SVG drawing machine.

@docs Env
@docs Warning
@docs unWarning
@docs getLocalToWorld, getWarnings, getNFixDigits, getStyle
@docs modStyle, applyStyleAtom
@docs getDefs, modDefs
@docs init
@docs thread

-}

import Techdraw.Internal.StyleAtom as StyleAtom exposing (StyleAtom)
import Techdraw.Internal.Svg.Defs as Defs exposing (Defs)
import Techdraw.Internal.Svg.Path exposing (NFixDigits(..))
import Techdraw.Math as Math exposing (AffineTransform)
import Techdraw.Style as Style exposing (Style)
import Techdraw.Types as T exposing (Sizing(..))


{-| Environment.
-}
type Env msg
    = Env
        { localToWorld : AffineTransform
        , warnings : List Warning
        , nFixDigits : NFixDigits
        , style : Style
        , defs : Defs
        }


{-| Return the local-to-world affine transformation from the environment.
-}
getLocalToWorld : Env msg -> AffineTransform
getLocalToWorld (Env env) =
    env.localToWorld


{-| Return the warnings from the environment.
-}
getWarnings : Env msg -> List Warning
getWarnings (Env env) =
    env.warnings


{-| Get the number of fixed digits for Float formatting from the environment.
-}
getNFixDigits : Env msg -> NFixDigits
getNFixDigits (Env env) =
    env.nFixDigits


{-| Return the style from the environment.
-}
getStyle : Env msg -> Style
getStyle (Env env) =
    env.style


{-| Modify the style in the environment.
-}
modStyle : (Style -> Style) -> Env msg -> Env msg
modStyle styleFn (Env oldEnv) =
    Env { oldEnv | style = styleFn oldEnv.style }


{-| Apply a `StyleAtom` to update the style in the environment.
-}
applyStyleAtom : StyleAtom -> Env msg -> Env msg
applyStyleAtom styleAtom =
    modStyle (StyleAtom.apply styleAtom)


{-| Return the SVG definitions from the environment.
-}
getDefs : Env msg -> Defs
getDefs (Env env) =
    env.defs


{-| Modify the `Defs` inside the environment.
-}
modDefs : (Defs -> Defs) -> Env msg -> Env msg
modDefs modFn (Env oldEnv) =
    Env { oldEnv | defs = modFn oldEnv.defs }


{-| Warning string.
-}
type Warning
    = Warning String


{-| Un-apply the `Warning` newtype.
-}
unWarning : Warning -> String
unWarning (Warning str) =
    str


{-| Create an initial drawing environment using the sizing information.
-}
init : Sizing -> Env msg
init sizing =
    Env
        { localToWorld = initLocalToWorld sizing
        , warnings = []
        , nFixDigits = NFixDigits 2
        , style = Style.inheritAll
        , defs = Defs.empty
        }


{-| Create an initial local-to-world transformation using the sizing
information.
-}
initLocalToWorld : Sizing -> AffineTransform
initLocalToWorld sz =
    Math.affFromComponents
        [ -- Flip the y-axis.
          Math.AffineScaling <|
            Math.Scaling 1 -1
        , Math.AffineTranslation <|
            Math.Translation <|
                Math.v2 0 (sz |> T.sizingGetViewBox |> T.viewBoxGetHeight)

        -- Set the correct origin.
        , Math.AffineTranslation <|
            Math.Translation <|
                Math.v2
                    (sz |> T.sizingGetViewBox |> T.viewBoxGetXMin)
                    (sz |> T.sizingGetViewBox |> T.viewBoxGetYMin)
        ]


{-| Thread an environment from ancestor to next in the depthwise traversal
order.

This determines which parts of the environment inherit parent-to-child vs
which are threaded through the depthwise traversal of the tree.

  - Any part of the environment which comes from `ancestor` is inherited
    parent-to-child,
  - Any part of the environment which comes from `next` is threaded through
    the depthwise traversal.

-}
thread : Env msg -> Env msg -> Env msg
thread (Env ancestor) (Env next) =
    Env
        { localToWorld = ancestor.localToWorld
        , warnings = next.warnings
        , nFixDigits = ancestor.nFixDigits
        , style = ancestor.style
        , defs = next.defs
        }
