module MinArr exposing
    ( push, removeAt, insertAt, extend, extendN
    , isLength
    )

{-| An `Arr` with at least a minimum amount of elements.


## modify

@docs push, removeAt, insertAt, extend, extendN


## scan


### compare

@docs isLength

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


push :
    element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin (Nat1Plus min)) element
push element =
    Internal.push element


insertAt :
    Nat (In indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength maybeN) element
    -> Arr (ValueMin (Nat2Plus minLengthMinus1)) element
insertAt index direction inserted =
    Internal.insertAt index direction inserted


removeAt :
    Nat (In indexMin minLengthMinus1 indexMaybeExact)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength maybeN) element
    -> Arr (ValueIn minLengthMinus1 maxLength) element
removeAt index direction =
    Internal.removeAt index direction


extend :
    Arr (In extensionMin extensionMax extensionMaybeN) element
    -> Nat (N extensionMin (Is min To extendedMin) x)
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin extendedMin) element
extend extension extensionMin =
    Internal.extend extension extensionMin


{-| Extend the `Arr (Min ...)` with a fixed length `Arr (N ...)`.

    Arr.from4 1 2 3 4
        |> NArr.toMin
        |> MinArr.extendN (Arr.from3 5 6 7)
    --> Arr [ 1, 2, 3, 4, 5, 6, 7 ]

-}
extendN :
    Arr (N added (Is min To sumMin) x) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin sumMin) element
extendN nArrayExtension =
    Internal.extendN nArrayExtension


replaceAt :
    Nat (In indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) max maybeN) element
    -> Arr (In (Nat1Plus minLengthMinus1) max maybeN) element
replaceAt index direction new =
    Internal.Arr.replaceAt index direction new



-- ## scan
-- ### compare


isLength :
    Nat
        (N
            (Nat1Plus triedMinus1)
            (Is (Nat1Plus aMinus1) To (Nat1Plus triedMinus1PlusA))
            x
        )
    -> { min : Nat (N min (Is lessRange To triedMinus1) y) }
    ->
        { equal :
            Arr
                (N
                    (Nat1Plus triedMinus1)
                    (Is (Nat1Plus aMinus1) To (Nat1Plus triedMinus1PlusA))
                    x
                )
                element
            -> result
        , greater : Arr (ValueMin (Nat2Plus triedMinus1)) element -> result
        , less : Arr (ValueIn min triedMinus1PlusA) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLength length =
    Internal.isLength length


isLengthAtLeast :
    Nat (In tried (Nat1Plus triedMinus1PlusA) triedMaybeN)
    -> { min : Nat (N min (Is minToTriedMin To tried) x) }
    ->
        { equalOrGreater : Arr (ValueIn tried max) element -> result
        , less : Arr (ValueIn min triedMinus1PlusA) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtLeast tried min cases =
    Internal.isLengthAtLeast tried min cases



-- ## extra


{-| **group TODO not implemented**

    { groups : --the List divided into equal-sized InArrays
    , less : --values to the _right_ which aren't enough
    }

    [ 1, 2, 3, 4, 5, 6, 7 ]
        |> InArray.group nat5 FirstToLast
    --> { groups = [ InArray.from5 1 2 3 4 5 ]
    --> , less = [ 6, 7 ]
    --> }

-}
group :
    Nat (In (Nat1Plus minMinus1) max groupSizeMaybeN)
    -> LinearDirection
    -> Arr (In min max maybeN) element
    ->
        { groups : Arr (In (Nat1Plus minMinus1) max groupSizeMaybeN) element
        , less : Arr (ValueIn min max) element
        }
group groupSize direction =
    \arr ->
        arr
            |> isLengthAtLeast groupSize
                { equalOrGreater =
                    \egArr ->
                        egArr
                            |> Arr.take groupSize xs
                            :: group groupSize (drop k xs)
                , less =
                    \lessArr -> { groups = Arr.empty, less = lessArr }
                }
