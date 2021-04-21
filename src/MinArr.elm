module MinArr exposing
    ( push, removeAt, insertAt, extend, extendN
    , isLength, isLengthAtLeast
    , group
    )

{-| If the maximum length is a type variable,

    first :
        Arr (In (Nat1Plus minMinus1) max maybeN) element
        -> element

use these operations instead of the ones in `Arr` or `InArr`


## modify

@docs push, removeAt, insertAt, extend, extendN


## scan length

@docs isLength, isLengthAtLeast


## transform

@docs group

-}

import Arr exposing (Arr, SerializeError(..), length, toArray)
import Internal.Arr
import Internal.MinArr as Internal
import LinearDirection exposing (LinearDirection)
import MinNat
import NNats exposing (..)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import TypeNats exposing (..)



-- ## modify


{-| Put a new element after the others.

    arrWithAtLeast5Elements
        |> MinArr.push "becomes the last"
    --> is of type Arr (ValueMin Nat6) String

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
    --> is of type Arr (ValueMin Nat6) String

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

    Arr.from4 1 2 3 4
        |> NArr.toMin
        |> MinArr.extendN (Arr.from3 5 6 7)
    --> Arr [ 1, 2, 3, 4, 5, 6, 7 ]

-}
extend :
    Arr (In extensionMin extensionMax extensionMaybeN) element
    -> Nat (N extensionMin (Is min To extendedMin) x)
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin extendedMin) element
extend extension extensionMin =
    Internal.extend extension extensionMin


{-| Append a fixed length `Arr (N ...)`.

    Arr.from4 1 2 3 4
        |> NArr.toMin
        |> MinArr.extendN (Arr.from3 5 6 7)
    --> Arr [ 1, 2, 3, 4, 5, 6, 7 ]

-}
extendN :
    Arr (N added (Is min To sumMin) x) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin sumMin) element
extendN arrExtension =
    Internal.extendN arrExtension



-- ## scan
-- ### compare


isLength :
    Nat (In (Nat1Plus triedMinus1) (Nat1Plus atLeastTriedMinus1) maybeN)
    -> { min : Nat (N min (Is minToTriedMinus1 To triedMinus1) x) }
    ->
        { equal :
            Arr
                (In
                    (Nat1Plus triedMinus1)
                    (Nat1Plus atLeastTriedMinus1)
                    maybeN
                )
                element
            -> result
        , greater :
            Arr (ValueMin (Nat2Plus triedMinus1)) element -> result
        , less :
            Arr (In min atLeastTriedMinus1 e) element -> result
        }
    -> Arr (In min max e) element
    -> result
isLength length =
    Internal.isLength length


isLengthAtLeast :
    Nat (In tried (Nat1Plus triedMinus1PlusA) triedMaybeN)
    -> { min : Nat (N min (Is minToTriedMin To tried) x) }
    ->
        { equalOrGreater : Arr (ValueMin tried) element -> result
        , less : Arr (In min triedMinus1PlusA maybeN) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtLeast tried min cases =
    Internal.isLengthAtLeast tried min cases



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
