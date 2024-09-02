module Techdraw.Datum exposing
    ( DatumName(..), DatumPrefix(..)
    , datumNameToString
    , createStringName
    )

{-| Named datum points in a drawing.

@docs DatumName, DatumPrefix
@docs datumNameToString
@docs createStringName

-}


{-| Name of a datum.
-}
type DatumName
    = DatumName String


{-| Prefix to provide namespacing for datum names.
-}
type DatumPrefix
    = DatumPrefix String


{-| Convert a `DatumName` to a `String`.
-}
datumNameToString : DatumName -> String
datumNameToString (DatumName name) =
    name


{-| Convert a `DatumPrefix` to a `String`.
-}
datumPrefixToString : DatumPrefix -> String
datumPrefixToString (DatumPrefix str) =
    str


{-| Create the string name of a final datum point from its list of
prefixes and its datum name.
-}
createStringName : List DatumPrefix -> DatumName -> String
createStringName dps n =
    List.map datumPrefixToString dps
        |> String.concat
        |> (\prefix -> prefix ++ datumNameToString n)
