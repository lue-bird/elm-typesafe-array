module Internal.InArr exposing
    ( isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , extend, extendN, insertAt, push, removeAt, serialize, value
    )

{-| All functions must be tested a lot, especially the type signatures.
Try to reduce the amount of functions.


## scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost

-}

import Arr exposing (Arr, length, toArray)
import Array
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (In, Is, N, Nat, To, ValueIn, ValueN, ValueOnly)
import Serialize
import TypeNats exposing (..)
import Typed exposing (isChecked, tag, val)


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
        , greater :
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
            |> InNat.isInRange lowerBound
                upperBound
                min
                { inRange = withLength >> .inRange cases
                , greater = withLength >> .greater cases
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
        , equalOrGreater :
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
                    withLength >> .equalOrGreater cases
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
        , greater : Arr (In (Nat1Plus upperBound) max maybeN) element -> result
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
                    withLength >> .greater cases
                }


serialize :
    Nat (In minLowerBound upperBound lowerBoundMaybeN)
    -> Nat (In upperBound upperBoundPlusA upperBoundMaybeN)
    -> Serialize.Codec String element
    ->
        Serialize.Codec
            String
            (Arr (ValueIn minLowerBound upperBoundPlusA) element)
serialize lowerBound upperBound serializeElement =
    Serialize.array serializeElement
        |> Serialize.mapValid
            (\array ->
                Array.length array
                    |> Nat.isIntInRange lowerBound
                        upperBound
                        { less =
                            \() ->
                                Err
                                    "Array length was less than the expected minimum"
                        , greater =
                            \_ ->
                                Err "Array length was greater than the expected maximum"
                        , inRange =
                            \lengthInRange ->
                                tag { array = array, length = lengthInRange }
                                    |> isChecked Internal.Arr
                                    |> Ok
                        }
            )
            toArray
