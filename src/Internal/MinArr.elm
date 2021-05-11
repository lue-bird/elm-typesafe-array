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
import Nat exposing (ArgIn, In, Is, Min, N, Nat, Only, To)
import TypeNats exposing (..)
import Typed exposing (isChecked, tag)


push :
    element
    -> Arr (In min max) element
    -> Arr (Min (Nat1Plus min)) element
push element =
    Internal.push element (MinNat.add nat1)


insertAt :
    Nat (ArgIn indexMin lengthMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus lengthMinus1) max) element
    -> Arr (Min (Nat2Plus lengthMinus1)) element
insertAt index direction elementToInsert =
    Internal.insertAt index
        direction
        elementToInsert
        (MinNat.add nat1)


removeAt :
    Nat (ArgIn indexMin lengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus lengthMinus1) max) element
    -> Arr (In lengthMinus1 max) element
removeAt index direction =
    Internal.removeAt index direction (MinNat.sub nat1)


extend :
    Nat (N minAdded atLeastMinAdded_ (Is min To sumMin) is_)
    -> Arr (In minAdded maxAdded) element
    -> Arr (In min max) element
    -> Arr (Min sumMin) element
extend minAddedLength extension =
    Internal.extend extension
        (\_ -> MinNat.add minAddedLength)


drop :
    Nat (N dropped_ atLeastDropped_ (Is minTaken To min) is_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken max) element
drop droppedAmount direction =
    Internal.drop droppedAmount direction MinNat.sub



-- ## scan length


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
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (In (Nat1Plus valueMinus1) atLeastValue) element)
            (Arr (Min (Nat2Plus valueMinus1)) element)
isLength amount lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> MinNat.is amount lowest of
            Nat.Equal equal ->
                Nat.Equal (withLength equal)

            Nat.Greater greater ->
                Nat.Greater (withLength greater)

            Nat.Less less ->
                Nat.Less (withLength less)


isLengthAtLeast :
    Nat
        (N
            lowerBound
            (Nat1Plus atLeastLowerBoundMinus1)
            isA_
            isB_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is lowestToLowerBound_ To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest atLeastLowerBoundMinus1) element)
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
    Nat (N upperBound atLeastUpperBound isA_ isB_)
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToAtMostMin_ To upperBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest atLeastUpperBound) element)
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
