module Internal.MinArr exposing
    ( isLength, isLengthAtLeast, isLengthAtMost
    , push, removeAt, drop, extend, extendOnly, insertAt
    , value
    )

{-|


## scan length

@docs isLength, isLengthAtLeast, isLengthAtMost


## modify

@docs push, removeAt, drop, extend, extendOnly, insertAt

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
    Arr (In minAdded maxAdded) element
    -> Nat (ArgN minAdded (Is min To extendedMin) x)
    -> Arr (In min max) element
    -> Arr (Min extendedMin) element
extend added minAdded =
    Internal.extend added
        (\addedLen -> MinNat.add addedLen minAdded)


extendOnly :
    Nat (ArgN added (Is min To extendedMin) x)
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (Min extendedMin) element
extendOnly addedLength arrExtension =
    Internal.extend arrExtension
        (\_ -> MinNat.addN addedLength)


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
    -> { min : Nat (ArgN min (Is minToTriedMinus1 To triedMinus1) y) }
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In min triedMinus1PlusA) element)
            (Arr (Only (Nat1Plus triedMinus1)) element)
            (Arr (Min (Nat2Plus triedMinus1)) element)
isLength amount min =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> MinNat.is amount min of
            Nat.Equal () ->
                Nat.Equal
                    (withLength (amount |> InNat.value))

            Nat.Greater greater ->
                Nat.Greater (withLength greater)

            Nat.Less less ->
                Nat.Less (withLength less)


isLengthAtLeast :
    Nat (ArgN lowerBound (Is a To (Nat1Plus lowerBoundMinus1PlusA)) x)
    -> { min : Nat (ArgN min (Is minToTriedMin To lowerBound) y) }
    -> Arr (In min max) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In min lowerBoundMinus1PlusA) element)
            (Arr (Min lowerBound) element)
isLengthAtLeast lowerBound min =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> MinNat.isAtLeast lowerBound min of
            Nat.Below less ->
                Nat.Below
                    (withLength less)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater (withLength atLeast)


isLengthAtMost :
    Nat (ArgN upperBound (Is a To upperBoundPlusA) x)
    -> { min : Nat (ArgN min (Is minToAtMostMin To upperBound) y) }
    -> Arr (In min max) element
    ->
        Nat.AtMostOrAbove
            (Arr (In min upperBoundPlusA) element)
            (Arr (Min (Nat1Plus upperBound)) element)
isLengthAtMost upperBound min =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> MinNat.isAtMost upperBound min of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess (withLength atMost)

            Nat.Above above ->
                Nat.Above (withLength above)


value : Arr (In min max) element -> Arr (Min min) element
value =
    Internal.mapLength MinNat.value
        >> isChecked Internal.Arr
