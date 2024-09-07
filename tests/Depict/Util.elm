module Depict.Util exposing (unsafeForceMaybe)

{-| Utility functions.

@docs unsafeForceMaybe

-}


{-| Unsafely force a `Maybe a` to an `a`.

This is for internal cases where we're really, really sure that a `Maybe`
contains a `Just` value, and will effectively panic (`Debug.todo`) if
that assumption fails.

-}
unsafeForceMaybe : String -> Maybe a -> a
unsafeForceMaybe reason ma =
    case ma of
        Just x ->
            x

        Nothing ->
            Debug.todo <|
                "Unsafe force of unsafe Maybe failed: Violated assumption: "
                    ++ reason
