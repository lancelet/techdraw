module Techdraw.Internal.Svg.Env exposing
    ( Env
    , Warning
    , EventHandlerCapturedEnv(..)
    , unWarning
    , getLocalToWorld, getWarnings, getNFixDigits, getStyle
    , modStyle, applyStyleAtom
    , getDefs, modDefs
    , getInitLocalToWorld, getInitWorldToLocal
    , setLocalToWorldTransform, setLocalToWorldTransformAsInit, concatTransform
    , getEventHandlers, hasPendingEventHandlers, addEventHandler
    , removeEventHandlers
    , addHostEventHandler, getHostEventHandlers
    , tagCSys, getCSysDict
    , init
    , thread
    )

{-| Environment for SVG drawing machine.

@docs Env
@docs Warning
@docs EventHandlerCapturedEnv
@docs unWarning
@docs getLocalToWorld, getWarnings, getNFixDigits, getStyle
@docs modStyle, applyStyleAtom
@docs getDefs, modDefs
@docs getInitLocalToWorld, getInitWorldToLocal
@docs setLocalToWorldTransform, setLocalToWorldTransformAsInit, concatTransform
@docs getEventHandlers, hasPendingEventHandlers, addEventHandler
@docs removeEventHandlers
@docs addHostEventHandler, getHostEventHandlers
@docs tagCSys, getCSysDict
@docs init
@docs thread

-}

import Techdraw.Event exposing (EventHandler)
import Techdraw.Internal.CSysDict as CSysDict exposing (CSysDict)
import Techdraw.Internal.StyleAtom as StyleAtom exposing (StyleAtom)
import Techdraw.Internal.Svg.Defs as Defs exposing (Defs)
import Techdraw.Internal.Svg.Path exposing (NFixDigits(..))
import Techdraw.Math as Math exposing (AffineTransform)
import Techdraw.Style as Style exposing (Style)
import Techdraw.Types as T exposing (CSysName, Sizing(..))


{-| Environment.
-}
type Env msg
    = Env
        { sizing : Sizing
        , initTransform : AffineTransform
        , localToWorld : AffineTransform
        , warnings : List Warning
        , nFixDigits : NFixDigits
        , style : Style
        , defs : Defs
        , eventHandlers : List (EventHandlerCapturedEnv msg)
        , hostEventHandlers : List (EventHandlerCapturedEnv msg)
        , cSysDict : CSysDict
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


{-| Return the initial local-to-world transformation.
-}
getInitLocalToWorld : Env msg -> AffineTransform
getInitLocalToWorld (Env env) =
    env.initTransform


{-| Return the initial world-to-local transformation.
-}
getInitWorldToLocal : Env msg -> AffineTransform
getInitWorldToLocal =
    getInitLocalToWorld >> Math.affInvert


{-| Set the local-to-world transformation.

This completedly overrides the transformation. To concatenate a transformation,
use the [`concatTransform`](#concatTransform) function.

-}
setLocalToWorldTransform : AffineTransform -> Env msg -> Env msg
setLocalToWorldTransform affineTransform (Env oldEnv) =
    Env
        { oldEnv
            | localToWorld = affineTransform
        }


{-| Override the transform in the environment with the original transform.
-}
setLocalToWorldTransformAsInit : Env msg -> Env msg
setLocalToWorldTransformAsInit (Env env) =
    setLocalToWorldTransform env.initTransform (Env env)


{-| Concatenate a transformation with the current local-to-world
transformation. This post-multiplies the supplied transformation with the
local-to-world transform.
-}
concatTransform : AffineTransform -> Env msg -> Env msg
concatTransform childToParent (Env oldEnv) =
    Env
        { oldEnv
            | localToWorld =
                Math.affMatMul oldEnv.localToWorld childToParent
        }


{-| Check if the environment has pending event handlers.
-}
hasPendingEventHandlers : Env msg -> Bool
hasPendingEventHandlers (Env env) =
    List.isEmpty env.eventHandlers |> not


{-| Return the list of event handlers from the environment.
-}
getEventHandlers : Env msg -> List (EventHandlerCapturedEnv msg)
getEventHandlers (Env env) =
    env.eventHandlers


{-| Prepend an event handler to the environment
-}
addEventHandler : EventHandler msg -> Env msg -> Env msg
addEventHandler eventHandler (Env oldEnv) =
    let
        handlerWithCapturedEnv =
            EventHandlerCapturedEnv
                { eventHandler = eventHandler
                , localToWorld = oldEnv.localToWorld
                }
    in
    Env
        { oldEnv
            | eventHandlers = handlerWithCapturedEnv :: oldEnv.eventHandlers
        }


{-| Remove all event handlers.
-}
removeEventHandlers : Env msg -> Env msg
removeEventHandlers (Env oldEnv) =
    Env
        { oldEnv
            | eventHandlers = []
        }


{-| Prepend a host event handler to the environment.
-}
addHostEventHandler : EventHandler msg -> Env msg -> Env msg
addHostEventHandler eventHandler (Env oldEnv) =
    let
        handlerWithCapturedEnv =
            EventHandlerCapturedEnv
                { eventHandler = eventHandler
                , localToWorld = oldEnv.localToWorld
                }
    in
    Env
        { oldEnv
            | hostEventHandlers =
                handlerWithCapturedEnv :: oldEnv.hostEventHandlers
        }


{-| Return the host event handlers from the environment.
-}
getHostEventHandlers : Env msg -> List (EventHandlerCapturedEnv msg)
getHostEventHandlers (Env env) =
    env.hostEventHandlers


{-| Tag the current local-to-world coordinate system in the dictionary of
coordinate system names.
-}
tagCSys : CSysName -> Env msg -> Env msg
tagCSys name (Env oldEnv) =
    Env
        { oldEnv
            | cSysDict =
                CSysDict.insertLocalToWorld
                    name
                    oldEnv.localToWorld
                    oldEnv.cSysDict
        }


{-| Return the coordinate system dictionary from the environment.
-}
getCSysDict : Env msg -> CSysDict
getCSysDict (Env env) =
    env.cSysDict


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
    let
        initTransform =
            initLocalToWorld sizing
    in
    Env
        { sizing = sizing
        , initTransform = initTransform
        , localToWorld = initTransform
        , warnings = []
        , nFixDigits = NFixDigits 2
        , style = Style.inheritAll
        , defs = Defs.empty
        , eventHandlers = []
        , hostEventHandlers = []
        , cSysDict = CSysDict.empty
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
        { sizing = ancestor.sizing
        , initTransform = ancestor.initTransform
        , localToWorld = ancestor.localToWorld
        , warnings = next.warnings
        , nFixDigits = ancestor.nFixDigits
        , style = ancestor.style
        , defs = next.defs
        , eventHandlers = ancestor.eventHandlers
        , hostEventHandlers = next.hostEventHandlers
        , cSysDict = next.cSysDict
        }


{-| Event handler that has captured relevant parts of its current
environment.
-}
type EventHandlerCapturedEnv msg
    = EventHandlerCapturedEnv
        { eventHandler : EventHandler msg
        , localToWorld : AffineTransform
        }
