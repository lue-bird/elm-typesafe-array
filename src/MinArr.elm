module MinArr exposing
    ( push, removeAt, insertAt
    , append, prepend
    , drop
    , isLength, isLengthAtLeast, isLengthAtMost
    , value
    , serialize
    , Error, generalizeError, errorToString
    )

{-| If the maximum length is a type variable,

    first :
        Arr (In (Nat1Plus minMinus1_) max_) element
        -> element

use these operations instead of the ones in `Arr` or `InArr`


# modify

@docs push, removeAt, insertAt


## glue

@docs append, prepend


## part

@docs drop


# scan length

@docs isLength, isLengthAtLeast, isLengthAtMost


# transform


## drop information

@docs value


## serialize

@docs serialize


## error

@docs Error, generalizeError, errorToString

-}

import Arr exposing (Arr)
import Common
import InNat
import Internal
import LinearDirection exposing (LinearDirection)
import Nat exposing (ArgIn, In, Is, Min, N, Nat, To)
import Nats exposing (..)
import Serialize exposing (Codec)



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

    cons :
        element
        -> Arr (In minLength maxLength_) element
        -> Arr (Min (Nat1Plus minLength)) element
    cons =
        MinArr.insertAt nat0 FirstToLast

-}
insertAt :
    Nat (ArgIn indexMin_ minLength indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In minLength maxLength_) element
    -> Arr (Min (Nat1Plus minLength)) element
insertAt index direction inserted =
    Internal.minInsertAt index direction inserted


{-| Kick out the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    removeLast atLeast1Element =
        atLeast1Element
            |> MinArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> Arr (In minLengthMinus1 maxLength) element
removeAt index direction =
    Internal.minRemoveAt index direction


{-| Attach elements of an `Arr` to the right.

    Arr.from3 1 2 3
        |> MinArr.append nat3
            atLeast3Elements
    --> : Arr (Min Nat6) ...

-}
append :
    Nat (N minAdded atLeastMinAdded_ (Is min To sumMin) is_)
    -> Arr (In minAdded maxAdded_) element
    -> Arr (In min max_) element
    -> Arr (Min sumMin) element
append minAddedLength extension =
    Internal.minAppend minAddedLength extension


{-| Add elements of an `Arr` to the left.

    Arr.from3 1 2 3
        |> MinArr.prepend nat3
            atLeast3Elements
    --> : Arr (Min Nat6) ...

-}
prepend :
    Nat (N minAdded atLeastMinAdded_ (Is min To sumMin) is_)
    -> Arr (In minAdded maxAdded_) element
    -> Arr (In min max_) element
    -> Arr (Min sumMin) element
prepend minAddedLength extension =
    Internal.minPrepend minAddedLength extension


{-| Elements after a certain number of elements in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    atLeast6Elements
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
                    |> Arr.fromList
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
                    |> Arr.fromList
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
    Internal.toMinArr


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s with a minimum amount of elements.

    import Serialize exposing (Codec)


    -- we can't start if we have no worlds to choose from
    serializeSaves : Codec String (Arr (Min Nat1) World)
    serializeSaves =
        MinArr.serialize nat1
            -- if we just want a simple error string
            MinArr.errorToString
            serializeWorld

The encode/decode functions can be extracted if needed.

    encode : Arr (In (Nat1Plus minMinus1_) max_) World -> Bytes
    encode =
        Arr.lowerMinLength nat1
            >> MinArr.value
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
    ->
        ({ expected : { length : { atLeast : Nat (Min Nat0) } }
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error element
    -> Codec error (Arr (Min min) element)
serialize lowerBound toSerializeError serializeElement =
    Internal.serializeMin lowerBound toSerializeError serializeElement


type alias Error =
    { expected : { length : { atLeast : Nat (Min Nat0) } }
    , actual : { length : Nat (Min Nat0) }
    }


generalizeError : Error -> Arr.Error
generalizeError error =
    error
        |> Common.generalizeError
            (Arr.LengthInBound
                << InNat.ExpectAtLeast
                << .atLeast
            )


{-| Convert the [serialization](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) error into a readable message.

    { expected = { length = { atLeast = nat11 } }
    , actual = { length = 10 }
    }
        |> MinArr.errorToString
    --> expected an array of length >= 11 but the actual length was 10

(example doesn't compile)

Equivalent to

    error
        |> MinArr.generalizeError
        |> Arr.errorToString

-}
errorToString : Error -> String
errorToString error =
    error
        |> generalizeError
        |> Arr.errorToString
