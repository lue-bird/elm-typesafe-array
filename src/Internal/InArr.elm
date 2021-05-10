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
import Nat exposing (ArgIn, ArgN, In, Is, Nat, Only, To)
import Serialize
import TypeNats exposing (..)
import Typed exposing (isChecked, tag, val)


push :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element (InNat.addN nat1)


insertAt :
    Nat (ArgIn indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> Arr (In (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction insertedElement =
    Internal.insertAt index
        direction
        insertedElement
        (InNat.addN nat1)


removeAt :
    Nat (ArgIn indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1)) element
    -> Arr (In minMinus1 maxMinus1) element
removeAt index direction =
    Internal.removeAt index direction (InNat.subN nat1)


extendIn :
    Nat (ArgN extensionMin (Is min To extendedMin) x)
    -> Nat (ArgN extensionMax (Is max To extendedMax) y)
    -> Arr (In extensionMin extensionMax) element
    -> Arr (In min max) element
    -> Arr (In extendedMin extendedMax) element
extendIn extensionMin extensionMax extension =
    Internal.extend extension
        (\extensionLen ->
            InNat.add extensionLen extensionMin extensionMax
        )


extend :
    Nat (ArgN added (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
extend addedLength extension =
    Internal.extend extension (\_ -> InNat.addN addedLength)


drop :
    Nat (ArgN dropped (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
drop droppedAmount direction =
    Internal.drop droppedAmount direction InNat.subN



-- ## scan length


isLength :
    Nat
        (ArgN
            value
            (Is valueToMax To max)
            (Is a To (Nat1Plus atLeastValueMinus1))
        )
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToValue To value)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (Only value) element)
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

            Nat.Equal () ->
                Nat.Equal (withLength (amount |> InNat.value))

            Nat.Greater greater ->
                Nat.Greater (withLength greater)


isLengthInRange :
    Nat
        (ArgN
            lowerBound
            (Is lowerBoundToLast To upperBound)
            (Is lowerBoundA To (Nat1Plus atLeastFirstMinus1))
        )
    -> Nat (ArgN upperBound (Is upperBoundToMax To max) (Is upperBoundA To atLeastLast))
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToFirst To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrInOrAboveRange
            (Arr (In lowest atLeastFirstMinus1) element)
            (Arr (In lowerBound atLeastLast) element)
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
        (ArgN
            lowerBound
            (Is a To (Nat1Plus atLeastLowerBoundMinus1))
            (Is atLeastRange To max)
        )
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is (Nat1Plus lessRange) To lowerBound)
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
        (ArgN
            upperBound
            (Is a To atLeastUpperBound)
            (Is (Nat1Plus greaterRange) To max)
        )
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToUpperBound To upperBound)
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
    Nat (ArgIn minLowerBound minUpperBound lowerBoundMaybeN)
    -> Nat (ArgIn minUpperBound maxUpperBound upperBoundMaybeN)
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
