module Techdraw.Anticipate exposing (..)

{-| A better version of `Expect` for testing.
-}

import Expect exposing (FloatingPointTolerance(..))
import Fuzz
import List.Nonempty as Nonempty exposing (Nonempty)
import Test


type Anticipate
    = Pass
    | Fail String


pass : Anticipate
pass =
    Pass


fail : String -> Anticipate
fail =
    Fail


isPass : Anticipate -> Bool
isPass a =
    case a of
        Pass ->
            True

        _ ->
            False


isFail : Anticipate -> Bool
isFail a =
    not (isPass a)


toString : Anticipate -> String
toString a =
    case a of
        Pass ->
            "Pass"

        Fail msg ->
            "Failure(s):\n" ++ msg



---- Test integration --------------------------------------------------------


dummy : Test.Test
dummy =
    test "dummy test" (\() -> pass)


test : String -> (() -> Anticipate) -> Test.Test
test name fn =
    Test.test name (\() -> fn () |> toExpectation)


fuzz : Fuzz.Fuzzer a -> String -> (a -> Anticipate) -> Test.Test
fuzz fuzzer name fn =
    Test.fuzz fuzzer name (\input -> fn input |> toExpectation)


fuzz2 :
    Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> String
    -> (a -> b -> Anticipate)
    -> Test.Test
fuzz2 fa fb name fn =
    Test.fuzz2 fa fb name (\a b -> fn a b |> toExpectation)


fuzz3 :
    Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> String
    -> (a -> b -> c -> Anticipate)
    -> Test.Test
fuzz3 fa fb fc name fn =
    Test.fuzz3 fa fb fc name (\a b c -> fn a b c |> toExpectation)


toExpectation : Anticipate -> Expect.Expectation
toExpectation a =
    case a of
        Pass ->
            Expect.pass

        Fail msg ->
            Expect.fail msg



---- Comparisons -------------------------------------------------------------


equal : a -> a -> Anticipate
equal x y =
    if x == y then
        pass

    else
        fail <| Debug.toString x ++ " == " ++ Debug.toString y


atLeast : comparable -> comparable -> Anticipate
atLeast boundary value =
    if value >= boundary then
        pass

    else
        fail <| Debug.toString value ++ " >= " ++ Debug.toString boundary


greaterThan : comparable -> comparable -> Anticipate
greaterThan boundary value =
    if value > boundary then
        pass

    else
        fail <| Debug.toString value ++ " > " ++ Debug.toString boundary


lessThan : comparable -> comparable -> Anticipate
lessThan boundary value =
    if value < boundary then
        pass

    else
        fail <| Debug.toString value ++ " < " ++ Debug.toString boundary


atMost : comparable -> comparable -> Anticipate
atMost boundary value =
    if value <= boundary then
        pass

    else
        fail <| Debug.toString value ++ " <= " ++ Debug.toString boundary


within : Expect.FloatingPointTolerance -> Float -> Float -> Anticipate
within tolerance x y =
    if withinCompare tolerance x y then
        pass

    else
        fail <| Debug.toString x ++ " ~= " ++ Debug.toString y


overList : (a -> a -> Anticipate) -> List a -> List a -> Anticipate
overList compare ls rs =
    if List.length ls /= List.length rs then
        fail "Lists were unequal length."

    else
        List.map2 compare ls rs |> all


overNonempty : (a -> a -> Anticipate) -> Nonempty a -> Nonempty a -> Anticipate
overNonempty compare ls rs =
    if Nonempty.length ls /= Nonempty.length rs then
        fail "Nonempty lists were unequal length."

    else
        Nonempty.map2 compare ls rs |> Nonempty.toList |> all



---- Combinators -------------------------------------------------------------


all : List Anticipate -> Anticipate
all ps =
    if List.all isPass ps then
        pass

    else
        fail
            ("Failures:\n"
                ++ (List.filter isFail ps
                        |> List.map toString
                        |> String.join "\n"
                   )
            )


any : List Anticipate -> Anticipate
any ps =
    if List.any isPass ps then
        pass

    else
        let
            failureStrings =
                List.filterMap
                    (\result ->
                        case result of
                            Pass ->
                                Nothing

                            Fail msg ->
                                Just msg
                    )
                    ps
        in
        fail <| "\nAll failed:\n" ++ String.join "\n" failureStrings


prependFailMsg : String -> Anticipate -> Anticipate
prependFailMsg msg a =
    case a of
        Pass ->
            a

        Fail tailMsg ->
            Fail <| msg ++ tailMsg


compareExtracted :
    String
    -> (parent -> a)
    -> (a -> a -> Anticipate)
    -> parent
    -> parent
    -> Anticipate
compareExtracted name extract compare expected actual =
    compare (extract expected) (extract actual)
        |> prependFailMsg (name ++ ": ")



---- Floating point comparisons ----------------------------------------------


absolute : Expect.FloatingPointTolerance -> Float
absolute tolerance =
    case tolerance of
        Expect.Absolute val ->
            val

        Expect.AbsoluteOrRelative val _ ->
            val

        _ ->
            0


relative : Expect.FloatingPointTolerance -> Float
relative tolerance =
    case tolerance of
        Expect.Relative val ->
            val

        Expect.AbsoluteOrRelative _ val ->
            val

        _ ->
            0


withinCompare : Expect.FloatingPointTolerance -> Float -> Float -> Bool
withinCompare tolerance a b =
    let
        withinAbsoluteTolerance =
            a - absolute tolerance <= b && b <= a + absolute tolerance

        withinRelativeTolerance =
            (a - abs (a * relative tolerance) <= b && b <= a + abs (a * relative tolerance))
                || (b - abs (b * relative tolerance) <= a && a <= b + abs (b * relative tolerance))
    in
    (a == b) || withinAbsoluteTolerance || withinRelativeTolerance
