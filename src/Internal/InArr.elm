module Internal.InArr exposing
    ( isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , drop, extend, extendOnly, insertAt, push, removeAt
    , serialize
    )

{-| All functions must be tested a lot, especially the type signatures.
Try to reduce the amount of functions.


## scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost


## modify

@docs drop, extend, extendOnly, insertAt, push, removeAt

-}

import Arr exposing (Arr, length, toArray)
import Array
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (ArgIn, ArgN, In, Is, N, Nat, Only, To)
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


extend :
    Arr (In extensionMin extensionMax) element
    -> Nat (ArgN extensionMin (Is min To extendedMin) x)
    -> Nat (ArgN extensionMax (Is max To extendedMax) y)
    -> Arr (In min max) element
    -> Arr (In extendedMin extendedMax) element
extend extension extensionMin extensionMax =
    Internal.extend extension
        (\extensionLen ->
            InNat.add extensionLen extensionMin extensionMax
        )


extendOnly :
    Nat (ArgN added (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
extendOnly addedLength extension =
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
            tried
            (Is triedToMax To max)
            (Is a To (Nat1Plus atLeastTriedMinus1))
        )
    -> { min : Nat (ArgN min (Is minToTried To tried) x) }
    ->
        { equal :
            Arr (Only tried) element
            -> result
        , greater :
            Arr (In (Nat2Plus triedMinus1) max) element -> result
        , less :
            Arr (In min atLeastTriedMinus1) element -> result
        }
    -> Arr (In min max) element
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
        (ArgN
            lowerBound
            (Is lowerBoundToLast To upperBound)
            (Is lowerBoundA To (Nat1Plus atLeastFirstMinus1))
        )
    -> Nat (ArgN upperBound (Is upperBoundToMax To max) (Is upperBoundA To atLeastLast))
    -> { min : Nat (ArgN min (Is minToFirst To lowerBound) x) }
    ->
        { inRange :
            Arr (In lowerBound atLeastLast) element
            -> result
        , less :
            Arr (In min atLeastFirstMinus1) element
            -> result
        , greater :
            Arr (In (Nat1Plus upperBound) max) element
            -> result
        }
    -> Arr (In min max) element
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
        (ArgN
            lowerBound
            (Is a To (Nat1Plus atLeastLowerBoundMinus1))
            (Is atLeastRange To max)
        )
    -> { min : Nat (ArgN min (Is (Nat1Plus lessRange) To lowerBound) x) }
    ->
        { less :
            Arr (In min atLeastLowerBoundMinus1) element
            -> result
        , equalOrGreater :
            Arr (In lowerBound max) element
            -> result
        }
    -> Arr (In min max) element
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
        (ArgN
            upperBound
            (Is a To atLeastUpperBound)
            (Is (Nat1Plus greaterRange) To max)
        )
    -> { min : Nat (ArgN min (Is minToUpperBound To upperBound) x) }
    ->
        { equalOrLess : Arr (In min atLeastUpperBound) element -> result
        , greater : Arr (In (Nat1Plus upperBound) max) element -> result
        }
    -> Arr (In min max) element
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
    Nat (ArgIn minLowerBound upperBound lowerBoundMaybeN)
    -> Nat (ArgIn upperBound upperBoundPlusA upperBoundMaybeN)
    -> Serialize.Codec String element
    ->
        Serialize.Codec
            String
            (Arr (In minLowerBound upperBoundPlusA) element)
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
