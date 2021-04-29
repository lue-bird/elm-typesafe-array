module Internal.InArr exposing
    ( isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , extend, extendN, insertAt, push, removeAt, value
    )

{-| All functions must be tested a lot, especially the type signatures.
Try to reduce the amount of functions.


## scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost

-}

import Arr exposing (Arr, length, toArray)
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (In, Is, N, Nat, To, ValueIn, ValueOnly)
import TypeNats exposing (..)
import Typed exposing (isChecked, tag)


push :
    element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element (InNat.addN nat1)


insertAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> Arr (ValueIn (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction insertedElement =
    Internal.insertAt index
        direction
        insertedElement
        (InNat.addN nat1)


removeAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1) maybeN) element
    -> Arr (ValueIn minMinus1 maxMinus1) element
removeAt index direction =
    Internal.removeAt index direction (InNat.subN nat1)


extend :
    Arr (In extensionMin extensionMax extensionMaybeN) element
    -> Nat (N extensionMin (Is min To extendedMin) x)
    -> Nat (N extensionMax (Is max To extendedMax) y)
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn extendedMin extendedMax) element
extend extension extensionMin extensionMax =
    Internal.extend extension
        (\extensionLen ->
            InNat.add extensionLen extensionMin extensionMax
        )


extendN :
    Arr (N added (Is min To sumMin) (Is max To sumMax)) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn sumMin sumMax) element
extendN nArrExtension =
    Internal.extend nArrExtension InNat.addN


value :
    Arr (In min max maybeExact) element
    -> Arr (ValueIn min max) element
value =
    Internal.mapLength InNat.value >> isChecked Internal.Arr



-- ## scan length


isLength :
    Nat
        (N
            tried
            (Is triedToMax To max)
            (Is a To (Nat1Plus atLeastTriedMinus1))
        )
    -> { min : Nat (N min (Is minToTried To tried) x) }
    ->
        { equal :
            Arr (ValueOnly tried) element
            -> result
        , greater :
            Arr (In (Nat2Plus triedMinus1) max maybeN) element -> result
        , less :
            Arr (In min atLeastTriedMinus1 maybeN) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLength amount min cases =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        length arr
            |> InNat.is amount
                min
                { equal =
                    \() ->
                        withLength (amount |> InNat.value)
                            |> .equal cases
                , greater = withLength >> .greater cases
                , less = withLength >> .less cases
                }


isLengthInRange :
    Nat
        (N
            lowerBound
            (Is lowerBoundToLast To upperBound)
            (Is lowerBoundA To (Nat1Plus atLeastFirstMinus1))
        )
    -> Nat (N upperBound (Is upperBoundToMax To max) (Is upperBoundA To atLeastLast))
    -> { min : Nat (N min (Is minToFirst To lowerBound) x) }
    ->
        { inRange :
            Arr (In lowerBound atLeastLast maybeN) element
            -> result
        , less :
            Arr (In min atLeastFirstMinus1 maybeN) element
            -> result
        , more :
            Arr (In (Nat1Plus upperBound) max maybeN) element
            -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthInRange lowerBound upperBound min cases =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        length arr
            |> InNat.isInRange { first = lowerBound, last = upperBound }
                min
                { inRange = withLength >> .inRange cases
                , greater = withLength >> .more cases
                , less = withLength >> .less cases
                }


isLengthAtLeast :
    Nat
        (N
            lowerBound
            (Is a To (Nat1Plus atLeastLowerBoundMinus1))
            (Is atLeastRange To max)
        )
    -> { min : Nat (N min (Is (Nat1Plus lessRange) To lowerBound) x) }
    ->
        { less :
            Arr (In min atLeastLowerBoundMinus1 maybeN) element
            -> result
        , equalOrMore :
            Arr (In lowerBound max maybeN) element
            -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtLeast lowerBound min cases =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        length arr
            |> InNat.isAtLeast lowerBound
                min
                { less = withLength >> .less cases
                , equalOrGreater =
                    withLength >> .equalOrMore cases
                }


isLengthAtMost :
    Nat
        (N
            upperBound
            (Is a To atLeastUpperBound)
            (Is (Nat1Plus greaterRange) To max)
        )
    -> { min : Nat (N min (Is minToUpperBound To upperBound) x) }
    ->
        { equalOrLess : Arr (In min atLeastUpperBound maybeN) element -> result
        , more : Arr (In (Nat1Plus upperBound) max maybeN) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtMost upperBound min cases =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        length arr
            |> InNat.isAtMost upperBound
                min
                { equalOrLess = withLength >> .equalOrLess cases
                , greater =
                    withLength >> .more cases
                }
