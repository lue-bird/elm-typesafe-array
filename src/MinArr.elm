module MinArr exposing
    ( push, removeAt, insertAt, extend, drop
    , isLength, isLengthAtLeast, isLengthAtMost
    , value
    , serialize
    )

{-| If the maximum length is a type variable,

    first :
        Arr (In (Nat1Plus minMinus1_) max_) element
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
import Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (ArgIn, In, Is, Min, N, Nat, To)
import Serialize
import TypeNats exposing (..)
import Typed exposing (val)



-- ## modify


{-| Put a new element after the others.

    atLeast5Elements
        |> MinArr.push "becomes the last"
    --> : Arr (Min Nat6) String

-}
push :
    element
    -> Arr (In minLength maxLength_) element
    -> Arr (Min (Nat1Plus minLength)) element
push element =
    Internal.minPush element


{-| Put a new element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    atLeast5Elements
        |> MinArr.insertAt nat0 LastToFirst
            "becomes the last"
    --> : Arr (Min Nat6) String

-}
insertAt :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength_) element
    -> Arr (Min (Nat2Plus minLengthMinus1)) element
insertAt index direction inserted =
    Internal.minInsertAt index direction inserted


{-| Kick out the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    removeLast arrWithAtLeast1Element =
        arrWithAtLeast1Element
            |> MinArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> Arr (In minLengthMinus1 maxLength) element
removeAt index direction =
    Internal.minRemoveAt index direction


{-| Append an `Arr`.

    Arr.from3 1 2 3
        |> MinArr.extend nat3 arrWithAtLeast3Elements
    --> : Arr (Min Nat6) ...

-}
extend :
    Nat (N minAdded atLeastMinAdded_ (Is min To sumMin) is_)
    -> Arr (In minAdded maxAdded) element
    -> Arr (In min max_) element
    -> Arr (Min sumMin) element
extend minAddedLength extension =
    Internal.minExtend minAddedLength extension


{-| Elements after a certain number of elements in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    withAtLeast6Elements
        |> MinArr.drop nat2 LastToFirst
    --> : Arr (Min Nat4) ...

-}
drop :
    Nat (N dropped_ atLeastDropped_ (Is minTaken To min) is_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken max) element
drop droppedAmount direction =
    Internal.minDrop droppedAmount direction



-- ## scan
-- ### scan length


{-| Compare its length to a given exact length. Is it `LessOrEqualOrGreater`?

`lowest` can be a number <= the minimum length.

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
        case
            wordArr
                |> MinArr.isLength nat3 { lowest = nat0 }
        of
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
    Nat
        (N
            (Nat1Plus valueMinus1)
            atLeastValue
            (Is a_ To (Nat1Plus atLeastValueMinus1))
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToValueMinus1_ To valueMinus1)
                )
        }
    -> Arr (In min max_) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (In (Nat1Plus valueMinus1) atLeastValue) element)
            (Arr (Min (Nat2Plus valueMinus1)) element)
isLength lengthToCompareAgainst =
    Internal.minIsLength lengthToCompareAgainst


{-| Compare its length to a given exact length.
Is it `BelowOrAtLeast` that value?

`lowest` can be a number <= the minimum length.

    convertUserArguments : String -> Result String (Arr (Min Nat3))
    convertUserArguments arguments =
        let
            wordArr =
                String.words arguments
                    |> Array.fromList
                    |> Arr.fromArray
        in
        case
            wordArr
                |> MinArr.isLengthAtLeast nat3 { lowest = nat0 }
        of
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
    Nat
        (ArgIn
            minLowerBound
            (Nat1Plus maxLowerBoundMinus1)
            ifN_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is lowestToMinLowerBound_ To minLowerBound)
                )
        }
    -> Arr (In min max_) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest maxLowerBoundMinus1) element)
            (Arr (Min minLowerBound) element)
isLengthAtLeast lowerBound lowest =
    Internal.minIsLengthAtLeast lowerBound lowest


{-| Is its length `AtMostOrAbove` a given length?

`lowest` can be a number <= the minimum length.

    -- only up to 50 tags
    tag : Arr (In min Nat50) String -> a -> Tagged a

    tagIfValidTags : Array String -> a -> Maybe (Tagged a)
    tagIfValidTags array value =
        case
            array
                |> Arr.fromArray
                |> MinArr.isLengthAtMost nat50 { lowest = nat0 }
        of
            Nat.EqualOrLess atMost50 ->
                tag value atMost50 |> Just

            Nat.Above _ ->
                Nothing

-}
isLengthAtMost :
    Nat (ArgIn minUpperBound maxUpperBound ifN_)
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is lowestToMinUpperBound_ To minUpperBound)
                )
        }
    -> Arr (In min max_) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest maxUpperBound) element)
            (Arr (Min (Nat1Plus minUpperBound)) element)
isLengthAtMost upperBound lowest =
    Internal.minIsLengthAtMost upperBound lowest



-- ## drop information


{-| Convert the `Arr (In min ...)` to a `Arr (Min min)`.

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
value : Arr (In min max_) element -> Arr (Min min) element
value =
    Internal.minValue


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s with a minimum amount of elements.

    import Serialize


    -- we can't start if we have no worlds to choose from!
    serializeSaves : Serialize.Codec String (Arr (Min Nat1) World)
    serializeSaves =
        MinArr.serialize nat1 serializeWorld

    encode : Arr (In (Nat1Plus minMinus1_) max_) World -> Bytes
    encode =
        MinArr.value
            >> Arr.lowerMinLength nat1
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
    Nat (ArgIn min max_ ifN_)
    -> Serialize.Codec String element
    -> Serialize.Codec String (Arr (Min min) element)
serialize lowerBound serializeElement =
    Internal.serializeMin lowerBound serializeElement
