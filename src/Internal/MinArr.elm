module Internal.MinArr exposing
    ( isLength, isLengthAtLeast, isLengthAtMost
    , push, removeAt, drop, extend, insertAt
    , value
    )

{-|


## scan length

@docs isLength, isLengthAtLeast, isLengthAtMost


## modify

@docs push, removeAt, drop, extend, insertAt

-}

import Arr exposing (Arr, length, toArray)
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import MinNat
import NNats exposing (..)
import Nat exposing (ArgIn, ArgN, In, Is, Min, Nat, Only, To)
import TypeNats exposing (..)
import Typed exposing (isChecked, tag)


push :
    element
    -> Arr (In min max) element
    -> Arr (Min (Nat1Plus min)) element
push element =
    Internal.push element (MinNat.addN nat1)


insertAt :
    Nat (ArgIn indexMin lengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus lengthMinus1) max) element
    -> Arr (Min (Nat2Plus lengthMinus1)) element
insertAt index direction elementToInsert =
    Internal.insertAt index
        direction
        elementToInsert
        (MinNat.addN nat1)


removeAt :
    Nat (ArgIn indexMin lengthMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus lengthMinus1) max) element
    -> Arr (In lengthMinus1 max) element
removeAt index direction =
    Internal.removeAt index direction (MinNat.subN nat1)


extend :
    Nat (ArgN minAdded (Is min To sumMin) x)
    -> Arr (In minAdded maxAdded) element
    -> Arr (In min max) element
    -> Arr (Min sumMin) element
extend minAddedLength extension =
    Internal.extend extension
        (\_ -> MinNat.addN minAddedLength)


drop :
    Nat (ArgN dropped (Is minTaken To min) x)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken max) element
drop droppedAmount direction =
    Internal.drop droppedAmount direction MinNat.subN



-- ## scan length


isLength :
    Nat (ArgN (Nat1Plus triedMinus1) (Is a To (Nat1Plus triedMinus1PlusA)) x)
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToTriedMinus1 To triedMinus1)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest triedMinus1PlusA) element)
            (Arr (Only (Nat1Plus triedMinus1)) element)
            (Arr (Min (Nat2Plus triedMinus1)) element)
isLength amount lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> MinNat.is amount lowest of
            Nat.Equal () ->
                Nat.Equal
                    (withLength (amount |> InNat.value))

            Nat.Greater greater ->
                Nat.Greater (withLength greater)

            Nat.Less less ->
                Nat.Less (withLength less)


isLengthAtLeast :
    Nat (ArgN lowerBound (Is a To (Nat1Plus lowerBoundMinus1PlusA)) x)
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToTriedMin To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest lowerBoundMinus1PlusA) element)
            (Arr (Min lowerBound) element)
isLengthAtLeast lowerBound lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> MinNat.isAtLeast lowerBound lowest of
            Nat.Below less ->
                Nat.Below
                    (withLength less)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater (withLength atLeast)


isLengthAtMost :
    Nat (ArgN upperBound (Is a To upperBoundPlusA) x)
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToAtMostMin To upperBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest upperBoundPlusA) element)
            (Arr (Min (Nat1Plus upperBound)) element)
isLengthAtMost upperBound lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> MinNat.isAtMost upperBound lowest of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess (withLength atMost)

            Nat.Above above ->
                Nat.Above (withLength above)


value : Arr (In min max) element -> Arr (Min min) element
value =
    Internal.mapLength MinNat.value
        >> isChecked Internal.Arr
