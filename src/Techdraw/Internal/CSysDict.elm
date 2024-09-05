module Techdraw.Internal.CSysDict exposing
    ( CSysDict
    , empty, insertLocalToWorld, getLocalToWorld, getWorldToLocal
    , inCSys
    )

{-| Dictionary (map) containing coordinate systems.

@docs CSysDict
@docs empty, insertLocalToWorld, getLocalToWorld, getWorldToLocal
@docs inCSys

-}

import Dict exposing (Dict)
import Techdraw.Math as Math exposing (AffineTransform, P2)
import Techdraw.Types exposing (CSysName(..))


{-| Map from the names of coordinate systems to their local-to-world
transformation.
-}
type CSysDict
    = CSysDict (Dict String AffineTransform)


{-| Empty coordinate system dictionary.
-}
empty : CSysDict
empty =
    CSysDict Dict.empty


{-| Insert a named coordinate system local-to-world transformation into the
dictionary.
-}
insertLocalToWorld : CSysName -> AffineTransform -> CSysDict -> CSysDict
insertLocalToWorld (CSysName name) localToWorld (CSysDict oldDict) =
    CSysDict <| Dict.insert name localToWorld oldDict


{-| Get a named coordinate system local-to-world transformation from the
dictionary.
-}
getLocalToWorld : CSysName -> CSysDict -> Maybe AffineTransform
getLocalToWorld (CSysName name) (CSysDict dict) =
    Dict.get name dict


{-| Get a named coordinate system world-to-local transformation from the
dictionary.
-}
getWorldToLocal : CSysName -> CSysDict -> Maybe AffineTransform
getWorldToLocal name =
    getLocalToWorld name >> Maybe.map Math.affInvert


{-| Convert a world-coordinate-system point to a point in the named
coordinate system. If the named coordinate system does not exist, a point
at (0,0) is returned.
-}
inCSys : CSysDict -> CSysName -> P2 -> P2
inCSys dict name worldPt =
    getWorldToLocal name dict
        |> Maybe.map (\w2l -> Math.p2ApplyAffineTransform w2l worldPt)
        |> Maybe.withDefault (Math.p2 0 0)
