module MinArr exposing
    ( push, removeAt, insertAt, extend, drop
    , isLength, isLengthAtLeast, isLengthAtMost
    , value
    , serialize
    )

{-| If the maximum length is a type variable,

    first :
        Arr (In (Nat1Plus orHigherMin) max) element
        -> element

use these operations instead of the ones in `Arr` or `InArr`


## modify

@docs push, removeAt, insertAt, extend, drop


## scan length

@docs isLength, isLengthAtLeast, isLengthAtMost


## drop information

@docs value


## extra

@docs serialize

-}

import Arr exposing (Arr, fromArray, length, toArray)
import Internal.MinArr as Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (ArgIn, ArgN, In, Is, Min, N, Nat, Only, To)
import Serialize
import TypeNats exposing (..)
import Typed exposing (val)



-- ## modify


{-| Put a new element after the others.

    arrWithAtLeast5Elements
        |> MinArr.push "becomes the last"
    --> : Arr (Min Nat6) String

-}
push :
    element
    -> Arr (In min max) element
    -> Arr (Min (Nat1Plus min)) element
push element =
    Internal.push element


{-| Put a new element at an index in a direction.

    arrWithAtLeast5Elements
        |> MinArr.insertAt nat0 FirstToLast
            "becomes the first"
    --> : Arr (Min Nat6) String

-}
insertAt :
    Nat (ArgIn indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> Arr (Min (Nat2Plus minLengthMinus1)) element
insertAt index direction inserted =
    Internal.insertAt index direction inserted


{-| Kick out the element at an index in a direction.

    removeLast arrWithAtLeast1Element =
        arrWithAtLeast1Element
            |> MinArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (ArgIn indexMin minLengthMinus1 indexMaybeExact)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> Arr (In minLengthMinus1 maxLength) element
removeAt index direction =
    Internal.removeAt index direction


{-| Append an `Arr (In ...)`.

    Arr.from3 1 2 3
        |> MinArr.extend nat3 arrWithAtLeast3Elements
    --> : Arr (Min Nat6) ...

-}
extend :
    Arr (In extensionMin extensionMax) element
    -> Nat (ArgN extensionMin (Is min To extendedMin) x)
    -> Arr (In min max) element
    -> Arr (Min extendedMin) element
extend extension extensionMin =
    Internal.extend extension extensionMin


{-| Elements after a certain number of elements from one side.

    withAtLeast6Elements
        |> Arr.drop nat2 LastToFirst
    --> : Arr (Min Nat4) ...

-}
drop :
    Nat (ArgN dropped (Is minTaken To min) x)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken max) element
drop droppedAmount direction =
    Internal.drop droppedAmount direction



-- ## scan
-- ### scan length


{-| Compare the length to an exact `Nat (ArgN ...)`. Is it `LessOrEqualOrGreater`?

`min` ensures that that `Nat (ArgN ...)` is bigger than the minimum length.

    convertUserArguments :
        String
        -> Result String (Arr (Only Nat3))
    convertUserArguments =
        let
            wordArr =
                String.words arguments
                    |> Array.fromList
                    |> Arr.fromArray
        in
        case wordArr |> MinArr.isLength nat3 { min = nat0 } of
            Nat.Equal only3 ->
                Ok only3

            Nat.Less atMost2 ->
                Err
                    ("I need 3 arguments, but there are only "
                        ++ String.fromInt (val (Arr.length atMost2))
                        ++ "."
                    )

            Nat.Greater atLeast4 ->
                Err
                    ("I need exact 3 arguments, so "
                        ++ String.fromInt (val (Arr.length atLeast4))
                        ++ " is too much."
                    )

-}
isLength :
    Nat (ArgN (Nat1Plus triedMinus1) (Is a To (Nat1Plus triedMinus1PlusA)) x)
    -> { min : Nat (ArgN min (Is minToTriedMinus1 To triedMinus1) y) }
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In min triedMinus1PlusA) element)
            (Arr (Only (Nat1Plus triedMinus1)) element)
            (Arr (Min (Nat2Plus triedMinus1)) element)
isLength length =
    Internal.isLength length


{-| Compare the length to an exact `Nat (ArgN ...)`.
Is it `BelowOrAtLeast` that value?

`min` ensures that that `Nat` is bigger than the minimum length.

    convertUserArguments : String -> Result String (Arr (Min Nat3))
    convertUserArguments arguments =
        let
            wordArr =
                String.words arguments
                    |> Array.fromList
                    |> Arr.fromArray
        in
        case wordArr |> MinArr.isLengthAtLeast nat3 { min = nat0 } of
            Nat.EqualOrGreater atLeast3 ->
                Ok atLeast3

            Nat.Below atMost2 ->
                Err
                    ("I need at least 3 arguments, but there are only "
                        ++ String.fromInt (val (Arr.length atMost2))
                        ++ "."
                    )

-}
isLengthAtLeast :
    Nat (ArgN lowerBound (Is a To (Nat1Plus lowerBoundMinus1PlusA)) x)
    -> { min : Nat (ArgN min (Is minToTriedMin To lowerBound) y) }
    -> Arr (In min max) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In min lowerBoundMinus1PlusA) element)
            (Arr (Min lowerBound) element)
isLengthAtLeast lowerBound min =
    Internal.isLengthAtLeast lowerBound min


{-| Is the length `AtMostOrAbove` a given `Nat`?

`min` ensures that that `Nat` is greater than the minimum length.

    -- only up to 50 tags
    tag : Arr (In min Nat50) String -> a -> Tagged a

    tagIfValidTags : Array String -> a -> Maybe (Tagged a)
    tagIfValidTags array value =
        case
            (array |> Arr.fromArray)
                |> MinArr.isLengthAtMost nat50 { min = nat0 }
        of
            Nat.EqualOrLess atMost50 ->
                tag value atMost50 |> Just

            Nat.Above _ ->
                Nothing

-}
isLengthAtMost :
    Nat (ArgN upperBound (Is a To upperBoundPlusA) x)
    -> { min : Nat (ArgN min (Is minToAtMostMin To upperBound) y) }
    -> Arr (In min max) element
    ->
        Nat.AtMostOrAbove
            (Arr (In min upperBoundPlusA) element)
            (Arr (Min (Nat1Plus upperBound)) element)
isLengthAtMost upperBound min =
    Internal.isLengthAtMost upperBound min



-- ## drop information


{-| Convert an exact Arr (In min ...) to a Nat (Min min).

    between4And10Elements |> MinArr.value
    --> : Arr (Min Nat4) ...

There is only 1 situation you should use this.

To make these the same type.

    [ atLeast1Element, between1And10Elements ]

Elm complains:

> But all the previous elements in the list are
> `Arr (Min Nat1) ...`

    [ atLeast1Element
    , between1And10Elements |> MinArr.value
    ]

-}
value : Arr (In min max) element -> Arr (Min min) element
value =
    Internal.value


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s with a minimum amount of elements.

    import Serialize


    -- we can't start if we have no worlds to choose from!
    serializeSaves :
        Serialize.Codec
            String
            (Arr (Min Nat1) World)
    serializeSaves =
        MinArr.serialize nat1 serializeWorld

    encode : Arr (In (Nat1Plus minMinus1) max) World -> Bytes
    encode =
        MinArr.value
            >> Serialize.encodeToBytes serializeSaves

    decode :
        Bytes
        ->
            Result
                (Serialize.Error String)
                (Arr (Min Nat1) World)
    decode =
        Serialize.decodeFromBytes serializeSaves

-}
serialize :
    Nat (ArgN lowerBound (Is a To (Nat1Plus lowerBoundMinus1PlusA)) x)
    -> Serialize.Codec String element
    -> Serialize.Codec String (Arr (Min lowerBound) element)
serialize lowerBound serializeElement =
    Serialize.array serializeElement
        |> Serialize.mapValid
            (\array ->
                case
                    fromArray array
                        |> isLengthAtLeast lowerBound
                            { min = nat0 }
                of
                    Nat.EqualOrGreater atLeast ->
                        Ok atLeast

                    Nat.Below below ->
                        Err
                            ("Array length "
                                ++ String.fromInt (val (length below))
                                ++ "was less than the required minimum"
                            )
            )
            toArray
