module MinArr exposing
    ( push, removeAt, insertAt, extend, extendN
    , isLength, isLengthAtLeast, isLengthAtMost
    , group
    , value
    )

{-| If the maximum length is a type variable,

    first :
        Arr (In (Nat1Plus minMinus1) max maybeN) element
        -> element

use these operations instead of the ones in `Arr` or `InArr`


## modify

@docs push, removeAt, insertAt, extend, extendN


## scan length

@docs isLength, isLengthAtLeast, isLengthAtMost


## transform

@docs group


## drop information

@docs value

-}

import Arr exposing (Arr, length, toArray)
import Internal.MinArr as Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (In, Is, N, Nat, To, ValueIn, ValueMin)
import TypeNats exposing (..)



-- ## modify


{-| Put a new element after the others.

    arrWithAtLeast5Elements
        |> MinArr.push "becomes the last"
    --> : Arr (ValueMin Nat6) String

-}
push :
    element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin (Nat1Plus min)) element
push element =
    Internal.push element


{-| Put a new element at an index in a direction.

    arrWithAtLeast5Elements
        |> MinArr.insertAt nat0 FirstToLast
            "becomes the first"
    --> : Arr (ValueMin Nat6) String

-}
insertAt :
    Nat (In indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength maybeN) element
    -> Arr (ValueMin (Nat2Plus minLengthMinus1)) element
insertAt index direction inserted =
    Internal.insertAt index direction inserted


{-| Kick out the element at an index in a direction.

    removeLast arrWithAtLeast1Element =
        arrWithAtLeast1Element
            |> MinArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (In indexMin minLengthMinus1 indexMaybeExact)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength maybeN) element
    -> Arr (ValueIn minLengthMinus1 maxLength) element
removeAt index direction =
    Internal.removeAt index direction


{-| Append an `Arr (In ...)`.

    Arr.from3 1 2 3
        |> MinArr.extend nat3 arrWithAtLeast3Elements
    --> : Arr (ValueMin Nat6) ...

-}
extend :
    Arr (In extensionMin extensionMax extensionMaybeN) element
    -> Nat (N extensionMin (Is min To extendedMin) x)
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin extendedMin) element
extend extension extensionMin =
    Internal.extend extension extensionMin


{-| Append a fixed length `Arr (N ...)`.

    arrWithAtLeast3Elements
        |> MinArr.extendN (Arr.from3 5 6 7)
    --> : Arr (ValueMin Nat6) ...

-}
extendN :
    Arr (N added (Is min To sumMin) x) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin sumMin) element
extendN arrExtension =
    Internal.extendN arrExtension



-- ## scan
-- ### scan length


{-| Compare the length to an exact `Nat (N ...)`.
Is it `greater`, `less` or `equal`?

`min` ensures that the `Nat (N ...)` is bigger than the minimum length.

    convertUserArguments :
        String
        -> Result String (Arr (ValueN Nat3 {- ... -}))
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
    Nat (In (Nat1Plus triedMinus1) (Nat1Plus atLeastTriedMinus1) triedMaybeN)
    -> { min : Nat (N min (Is minToTriedMinus1 To triedMinus1) x) }
    ->
        { equal :
            Arr
                (In
                    (Nat1Plus triedMinus1)
                    (Nat1Plus atLeastTriedMinus1)
                    triedMaybeN
                )
                element
            -> result
        , greater :
            Arr (ValueMin (Nat2Plus triedMinus1)) element -> result
        , less :
            Arr (In min atLeastTriedMinus1 maybeN) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLength length =
    Internal.isLength length


{-| Compare the length to an exact `Nat (N ...)`.
Is it `equalOrGreater` or `less`?

`min` ensures that the lower bound is bigger than the minimum length.

    convertUserArguments :
        String
        -> Result String (Arr (ValueMin Nat3))
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
    Nat (In lowerBound (Nat1Plus lowerBoundMinus1PlusA) lowerBoundMaybeN)
    -> { min : Nat (N min (Is minToTriedMin To lowerBound) x) }
    ->
        { equalOrGreater : Arr (ValueMin lowerBound) element -> result
        , less : Arr (In min lowerBoundMinus1PlusA maybeN) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtLeast lowerBound min cases =
    Internal.isLengthAtLeast lowerBound min cases


{-| Is the length

  - `equalOrLess` than an upper bound or

  - `greater`?

`min` ensures that the upper bound is greater than the minimum length.

    -- only up to 50 tags
    tag :
        Arr (In min Nat50 maybeN) String
        -> a
        -> Tagged a

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
    Nat (In atMostMin atLeastAtMostMin atMostMaybeN)
    -> { min : Nat (N min (Is minToAtMostMin To atMostMin) x) }
    ->
        { equalOrLess :
            Arr (In min atLeastAtMostMin maybeN) element
            -> result
        , greater :
            Arr (ValueMin (Nat1Plus atMostMin)) element
            -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtMost tried min cases =
    Internal.isLengthAtMost tried min cases



-- ## extra


{-| Split

    { groups : the Arr divided into equal-sized Arrs
    , less : values to one side which aren't enough
    }

    Arr.from7 1 2 3 4 5 6 7
        |> MinArr.group nat5 FirstToLast
    --> { groups = Arr.from1 (Arr.from5 1 2 3 4 5)
    --> , less = Arr.from2 6 7
    --> }

The type of the result isn't as accurate as in the example, though!

-}
group :
    Nat (In (Nat1Plus minGroupSizMinus1) maxGroupSize groupSizeMaybeN)
    -> LinearDirection
    -> Arr (In min max maybeN) element
    ->
        { groups :
            Arr
                (ValueIn Nat0 max)
                (Arr
                    (In (Nat1Plus minGroupSizMinus1) maxGroupSize groupSizeMaybeN)
                    element
                )
        , less : Arr (ValueIn Nat0 max) element
        }
group groupSize direction =
    Internal.group groupSize direction



-- ## drop information


{-| Convert an exact Arr (In min ...) to a Nat (ValueMin min).

    between4And10Elements |> MinArr.value
    --> : Arr (ValueMin Nat4) ...

There is only 1 situation you should use this.

To make these the same type.

    [ atLeast1Element, between1And10Elements ]

Elm complains:

> But all the previous elements in the list are
> `Arr (ValueMin Nat1) ...`

    [ atLeast1Element
    , between1And10Elements |> MinArr.value
    ]

-}
value : Arr (In min max maybeN) element -> Arr (ValueMin min) element
value =
    Internal.value
