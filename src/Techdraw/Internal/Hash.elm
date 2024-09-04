module Techdraw.Internal.Hash exposing
    ( Hash, Hasher, Manifest, Encoder
    , toHex
    , fromEncoder
    , f32, u32
    , encHash
    , sequence, list
    , enc2, enc4, enc6
    , attachTag
    , color
    , p2, v2, m22, affineTransform
    )

{-| Hashing values.

To hash a value, there are typically two steps:

1.  Create an [`Encoder`](#Encoder) for the type.
2.  Use the [`fromEncoder`](#fromEncoder) function to create a hasher.

For example, suppose we have our own two-element record called `G`. We
might create a [`Hasher`](#Hasher) for it like this:

    type alias G
        = { x : Float, y : Float }

    encG : Encoder G
    encG =
        enc2 f32 f32 .x .y |> attachTag "MyModule.G"

    hashG : Hasher G
    hashG =
        fromEncoder encG

    hashG {x = 1, y = 2} |> toHex
    --> "997277d2368dc71980e42a29255275a33544f449"

    hashG {x = 2, y = 1} |> toHex
    --> "6b0444634a826e716b090fdae9c183bc5f41af36"

The hashing works by first encoding the value to a binary blob, and then
running a SHA1 algorithm over the blob to produce a hash.


# Hashing Types and Functions

@docs Hash, Hasher, Manifest, Encoder
@docs toHex
@docs fromEncoder
@docs f32, u32
@docs encHash
@docs sequence, list
@docs enc2, enc4, enc6
@docs attachTag


# Encoders for Types

@docs color
@docs p2, v2, m22, affineTransform

-}

import Bytes as B
import Bytes.Encode as BE
import Color exposing (Color)
import SHA1
import Techdraw.Math as Math exposing (AffineTransform, M22, P2, V2)



---- Hashing Machinery --------------------------------------------------------


{-| A hash digest.

Use [`toHex`](#toHex) to convert a `Hash` to a `String`.

-}
type Hash
    = Hash SHA1.Digest


{-| A `Hasher` is a function which produces a hash from a type.
-}
type alias Hasher a =
    a -> Hash


{-| A `Manifest` records information from a type for hashing.
-}
type Manifest
    = Manifest BE.Encoder


{-| An `Encoder` takes a type and produces a `Manifest`.
-}
type alias Encoder a =
    a -> Manifest


{-| Convert a `Hash` value to a hex string.
-}
toHex : Hash -> String
toHex =
    unHash >> SHA1.toHex


{-| Unwrap the `Hash` newtype.
-}
unHash : Hash -> SHA1.Digest
unHash (Hash digest) =
    digest


{-| Unwrap the `Manifest` newtype.
-}
unManifest : Manifest -> BE.Encoder
unManifest (Manifest enc) =
    enc


{-| Produce a `Hasher` from an `Encoder`.
-}
fromEncoder : Encoder a -> Hasher a
fromEncoder encFn =
    encFn >> unManifest >> BE.encode >> SHA1.fromBytes >> Hash


{-| Default endianness.
-}
endianness : B.Endianness
endianness =
    B.LE


{-| Encoder for `f32` values.
-}
f32 : Encoder Float
f32 =
    BE.float32 endianness >> Manifest


{-| Encoder for `u32` values.
-}
u32 : Encoder Int
u32 =
    BE.unsignedInt32 endianness >> Manifest


{-| Re-encode a hash into another hash.
-}
encHash : Encoder Hash
encHash =
    \hash ->
        let
            { a, b, c, d, e } =
                unHash hash |> SHA1.toInt32s
        in
        List.map u32 [ a, b, c, d, e ] |> sequence


{-| Sequence a list of manifests into a single manifest.
-}
sequence : List Manifest -> Manifest
sequence =
    List.map unManifest >> BE.sequence >> Manifest


{-| Encode a list of items using an encoder for a single item.
-}
list : Encoder a -> Encoder (List a)
list itemEncoder items =
    (List.length items |> u32) :: List.map itemEncoder items |> sequence


{-| Attach a type tag to the front of an existing encoder.
-}
attachTag : String -> Encoder a -> Encoder a
attachTag tag encFn =
    \value ->
        sequence
            [ Manifest (BE.string tag)
            , encFn value
            ]


{-| Apply 2 encoders in sequence.
-}
enc2 :
    Encoder a
    -> Encoder b
    -> (z -> a)
    -> (z -> b)
    -> Encoder z
enc2 ea eb fa fb =
    applyAll [ fa >> ea, fb >> eb ] >> sequence


{-| Apply 4 encoders in sequence.
-}
enc4 :
    Encoder a
    -> Encoder b
    -> Encoder c
    -> Encoder d
    -> (z -> a)
    -> (z -> b)
    -> (z -> c)
    -> (z -> d)
    -> Encoder z
enc4 ea eb ec ed fa fb fc fd =
    applyAll [ fa >> ea, fb >> eb, fc >> ec, fd >> ed ] >> sequence


{-| Apply 6 encoders in sequence.
-}
enc6 :
    Encoder a
    -> Encoder b
    -> Encoder c
    -> Encoder d
    -> Encoder e
    -> Encoder f
    -> (z -> a)
    -> (z -> b)
    -> (z -> c)
    -> (z -> d)
    -> (z -> e)
    -> (z -> f)
    -> Encoder z
enc6 ea eb ec ed ee ef fa fb fc fd fe ff =
    applyAll
        [ fa >> ea
        , fb >> eb
        , fc >> ec
        , fd >> ed
        , fe >> ee
        , ff >> ef
        ]
        >> sequence


{-| Apply a list of functions to a value.
-}
applyAll : List (a -> b) -> a -> List b
applyAll =
    applyAllAccum []


{-| Apply a list of functions to a value using an explicit accumulator.
-}
applyAllAccum : List b -> List (a -> b) -> a -> List b
applyAllAccum accum fns value =
    case fns of
        [] ->
            List.reverse accum

        f :: fs ->
            applyAllAccum (f value :: accum) fs value



---- Hashers for Types --------------------------------------------------------


{-| Encoder for `Color`.
-}
color : Encoder Color
color =
    Color.toRgba
        >> enc4 f32 f32 f32 f32 .red .green .blue .alpha
        |> attachTag "Color.Color"


{-| Encoder for `P2`.
-}
p2 : Encoder P2
p2 =
    enc2 f32 f32 Math.p2x Math.p2y
        |> attachTag "Math.P2"


{-| Encoder for `V2`.
-}
v2 : Encoder V2
v2 =
    enc2 f32 f32 Math.v2e1 Math.v2e2
        |> attachTag "Math.V2"


{-| Encoder for `M22`.
-}
m22 : Encoder M22
m22 =
    enc4 f32 f32 f32 f32 Math.m22e11 Math.m22e12 Math.m22e21 Math.m22e22
        |> attachTag "Math.M22"


{-| Encoder for `AffineTransform`.
-}
affineTransform : Encoder AffineTransform
affineTransform =
    enc2 m22 v2 Math.affGetLinear Math.affGetTranslation
        |> attachTag "Math.AffineTransform"
