module Techdraw.Internal.Codec exposing
    ( endianness, encf32, decf32, encu32, decu32
    , encColor, decColor
    , encList, decList
    )

{-| Encoding and decoding from `Bytes`.

This functionality currently exists for the purposes of hashing.

@docs endianness, encf32, decf32, encu32, decu32
@docs encColor, decColor
@docs encList, decList

-}

import Bytes exposing (Endianness)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Color exposing (Color)



---- General ------------------------------------------------------------------


{-| Default endianness.
-}
endianness : Endianness
endianness =
    Bytes.BE


{-| Encode a `Float32` value with default endianness.
-}
encf32 : Float -> Encoder
encf32 =
    Encode.float32 endianness


{-| Decode a `Float32` value with default endianness.
-}
decf32 : Decoder Float
decf32 =
    Decode.float32 endianness


{-| Encode a u32 with default endianness.
-}
encu32 : Int -> Encoder
encu32 =
    Encode.unsignedInt32 endianness


{-| Decode a u32 with default endianness.
-}
decu32 : Decoder Int
decu32 =
    Decode.unsignedInt32 endianness



---- External Types -----------------------------------------------------------


{-| Encode a `Color`.
-}
encColor : Color -> Encoder
encColor color =
    let
        rgba =
            Color.toRgba color
    in
    Encode.sequence
        [ encf32 rgba.red
        , encf32 rgba.green
        , encf32 rgba.blue
        , encf32 rgba.alpha
        ]


{-| Decode a `Color`.
-}
decColor : Decoder Color
decColor =
    Decode.map4
        (\r g b a -> Color.fromRgba { red = r, green = g, blue = b, alpha = a })
        decf32
        decf32
        decf32
        decf32



---- List Encoders and Decoders -----------------------------------------------


{-| Encode a list prefixed by its length.
-}
encList : (a -> Encoder) -> List a -> Encoder
encList encElem lst =
    let
        n =
            List.length lst
    in
    Encode.sequence <| encu32 n :: List.map encElem lst


{-| Decode a list prefixed by its length.
-}
decList : Decoder a -> Decoder (List a)
decList decElem =
    decu32 |> Decode.andThen (decListN decElem)


{-| Decode a list with a decoder for one element and an element count.
-}
decListN : Decoder a -> Int -> Decoder (List a)
decListN decElem nElem =
    Decode.loop ( nElem, [] )
        (\( nLeft, accum ) ->
            if nLeft == 0 then
                Decode.succeed <| Decode.Done <| List.reverse accum

            else
                Decode.map (\x -> Decode.Loop ( nLeft - 1, x :: accum )) decElem
        )
