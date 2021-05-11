module Internal.InArr exposing
    ( isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , drop, extend, extendIn, insertAt, push, removeAt
    , serialize
    )

{-| All functions must be tested a lot, especially the type signatures.
Try to reduce the amount of functions.


## scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost


## modify

@docs drop, extend, extendIn, insertAt, push, removeAt

-}

import Arr exposing (Arr, length, toArray)
import Array
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (ArgIn, In, Is, N, Nat, Only, To)
import Serialize
import TypeNats exposing (..)
import Typed exposing (isChecked, tag, val)


push :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element (InNat.add nat1)


insertAt :
    Nat (ArgIn indexMin minMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> Arr (In (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction insertedElement =
    Internal.insertAt index
        direction
        insertedElement
        (InNat.add nat1)


removeAt :
    Nat (ArgIn indexMin_ minMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1)) element
    -> Arr (In minMinus1 maxMinus1) element
removeAt index direction =
    Internal.removeAt index direction (InNat.sub nat1)


extendIn :
    Nat (N addedMin atLeastAddedMin_ (Is min To extendedMin) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is max To extendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In min max) element
    -> Arr (In extendedMin extendedMax) element
extendIn extensionMin extensionMax extension =
    Internal.extend extension
        (InNat.addIn extensionMin extensionMax)


extend :
    Nat (N added atLeastAdded (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
extend addedLength extension =
    Internal.extend extension (\_ -> InNat.add addedLength)


drop :
    Nat (N dropped_ atLeastDropped_ (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
drop droppedAmount direction =
    Internal.drop droppedAmount direction InNat.sub



-- ## scan length


isLength :
    Nat
        (N
            (Nat1Plus valueMinus1)
            atLeastValue
            (Is a_ To (Nat1Plus atLeastValueMinus1))
            (Is valueToMax_ To max)
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToValue_ To (Nat1Plus valueMinus1))
                )
        }
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (In (Nat1Plus valueMinus1) atLeastValue) element)
            (Arr (In (Nat2Plus valueMinus1) max) element)
isLength amount lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case length arr |> InNat.is amount lowest of
            Nat.Less less ->
                Nat.Less (withLength less)

            Nat.Equal equal ->
                Nat.Equal (withLength equal)

            Nat.Greater greater ->
                Nat.Greater (withLength greater)


isLengthInRange :
    Nat
        (N
            lowerBound
            (Nat1Plus atLeastLowerBoundMinus1)
            (Is lowerBoundToUpperBound_ To upperBound)
            lowerBoundIs_
        )
    ->
        Nat
            (N
                upperBound
                atLeastUpperBound
                (Is upperBoundToMax_ To max)
                upperBoundIs_
            )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToLowerBound_ To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrInOrAboveRange
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) max) element)
isLengthInRange lowerBound upperBound lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case
            length arr
                |> InNat.isInRange lowerBound upperBound lowest
        of
            Nat.BelowRange below ->
                Nat.BelowRange (withLength below)

            Nat.InRange inRange ->
                Nat.InRange (withLength inRange)

            Nat.AboveRange above ->
                Nat.AboveRange (withLength above)


isLengthAtLeast :
    Nat
        (N
            lowerBound
            (Nat1Plus atLeastLowerBoundMinus1)
            (Is atLeastRange_ To max)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest
                    (Is lowestToMin To min)
                    (Is (Nat1Plus lowestToLowerBound_) To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound max) element)
isLengthAtLeast lowerBound lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case
            length arr
                |> InNat.isAtLeast lowerBound lowest
        of
            Nat.Below below ->
                Nat.Below (withLength below)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater (withLength atLeast)


isLengthAtMost :
    Nat
        (N
            upperBound
            atLeastUpperBound
            (Is (Nat1Plus greaterRange_) To max)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToUpperBound_ To upperBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) max) element)
isLengthAtMost upperBound lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        case
            length arr
                |> InNat.isAtMost upperBound lowest
        of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess (withLength atMost)

            Nat.Above above ->
                Nat.Above (withLength above)


serialize :
    Nat (ArgIn minLowerBound minUpperBound lowerBoundIfN_)
    -> Nat (ArgIn minUpperBound maxUpperBound upperBoundIfN_)
    -> Serialize.Codec String element
    ->
        Serialize.Codec
            String
            (Arr (In minLowerBound maxUpperBound) element)
serialize lowerBound upperBound serializeElement =
    Serialize.array serializeElement
        |> Serialize.mapValid
            (\array ->
                case
                    Array.length array
                        |> Nat.isIntInRange lowerBound upperBound
                of
                    Nat.BelowRange () ->
                        Err "Array length was less than the expected minimum"

                    Nat.AboveRange _ ->
                        Err "Array length was greater than the expected maximum"

                    Nat.InRange lengthInRange ->
                        tag { array = array, length = lengthInRange }
                            |> isChecked Internal.Arr
                            |> Ok
            )
            toArray
