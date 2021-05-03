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


{-| Compare the length to an exact `Nat (ArgN ...)`.
Is it `greater`, `less` or `equal`?

`min` ensures that the `Nat (ArgN ...)` is bigger than the minimum length.

    convertUserArguments :
        String
        -> Result String (Arr (Only Nat3))
    convertUserArguments =
        String.words
            >> Array.fromList
            >> Arr.fromArray
            >> MinArr.isLength nat3
                { min = nat0 }
                { equal = Ok
                , less =
                    \less ->
                        Err
                            ("I need 3 arguments, but there are only "
                                ++ String.fromInt (val (Arr.length less))
                                ++ "."
                            )
                , greater =
                    \more ->
                        Err
                            ("I need exact 3 arguments, so "
                                ++ String.fromInt (val (Arr.length more))
                                ++ " is too much."
                            )
                }

-}
isLength :
    Nat (ArgN (Nat1Plus triedMinus1) (Is a To (Nat1Plus triedMinus1PlusA)) x)
    -> { min : Nat (ArgN min (Is minToTriedMinus1 To triedMinus1) y) }
    ->
        { equal :
            Arr
                (Only (Nat1Plus triedMinus1))
                element
            -> result
        , greater :
            Arr (Min (Nat2Plus triedMinus1)) element -> result
        , less :
            Arr (In min triedMinus1PlusA) element -> result
        }
    -> Arr (In min max) element
    -> result
isLength length =
    Internal.isLength length


{-| Compare the length to an exact `Nat (ArgN ...)`.
Is it `equalOrGreater` or `less`?

`min` ensures that the lower bound is bigger than the minimum length.

    convertUserArguments :
        String
        -> Result String (Arr (Min Nat3))
    convertUserArguments =
        String.words
            >> Array.fromList
            >> Arr.fromArray
            >> MinArr.isLengthAtLeast nat3
                { min = nat0 }
                { equalOrGreater = Ok
                , less =
                    \less ->
                        Err
                            ("I need at least 3 arguments, but there are only "
                                ++ String.fromInt (val (Arr.length less))
                                ++ "."
                            )
                }

-}
isLengthAtLeast :
    Nat (ArgN lowerBound (Is a To (Nat1Plus lowerBoundMinus1PlusA)) x)
    -> { min : Nat (ArgN min (Is minToTriedMin To lowerBound) y) }
    ->
        { equalOrGreater : Arr (Min lowerBound) element -> result
        , less : Arr (In min lowerBoundMinus1PlusA) element -> result
        }
    -> Arr (In min max) element
    -> result
isLengthAtLeast lowerBound min cases =
    Internal.isLengthAtLeast lowerBound min cases


{-| Is the length

  - `equalOrLess` than an upper bound or

  - `greater`?

`min` ensures that the upper bound is greater than the minimum length.

    -- only up to 50 tags
    tag : Arr (In min Nat50) String -> a -> Tagged a

    tagIfValidTags : Array String -> a -> Maybe (Tagged a)
    tagIfValidTags array value =
        array
            |> Arr.fromArray
            |> MinArr.isLengthAtMost nat50
                { equalOrLess = tag value >> Just
                , greater = \_ -> Nothing
                }

-}
isLengthAtMost :
    Nat (ArgN upperBound (Is a To upperBoundPlusA) x)
    -> { min : Nat (ArgN min (Is minToAtMostMin To upperBound) y) }
    ->
        { equalOrLess :
            Arr (In min upperBoundPlusA) element
            -> result
        , greater :
            Arr (Min (Nat1Plus upperBound)) element
            -> result
        }
    -> Arr (In min max) element
    -> result
isLengthAtMost upperBound min cases =
    Internal.isLengthAtMost upperBound min cases



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
            (fromArray
                >> isLengthAtLeast lowerBound
                    { min = nat0 }
                    { equalOrGreater =
                        Ok
                    , less =
                        \_ -> Err "Array length was less than the required minimum"
                    }
            )
            toArray
