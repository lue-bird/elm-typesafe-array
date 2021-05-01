module Internal.MinArr exposing
    ( extend
    , extendN
    , group
    , insertAt
    , isLength
    , isLengthAtLeast
    , isLengthAtMost
    , push
    , removeAt
    , value
    )

import Arr exposing (Arr, length, toArray)
import Array
import Array.LinearDirection as Array
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import MinNat
import NNats exposing (..)
import Nat exposing (In, Is, N, Nat, To, ValueIn, ValueMin, ValueOnly)
import TypeNats exposing (..)
import Typed exposing (isChecked, tag, val)


push :
    element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin (Nat1Plus min)) element
push element =
    Internal.push element (MinNat.addN nat1)


insertAt :
    Nat (In indexMin lengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus lengthMinus1) max maybeN) element
    -> Arr (ValueMin (Nat2Plus lengthMinus1)) element
insertAt index direction elementToInsert =
    Internal.insertAt index
        direction
        elementToInsert
        (MinNat.addN nat1)


removeAt :
    Nat (In indexMin lengthMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus lengthMinus1) max maybeN) element
    -> Arr (ValueIn lengthMinus1 max) element
removeAt index direction =
    Internal.removeAt index direction (MinNat.subN nat1)


extend :
    Arr (In minAdded maxAdded addedMaybeN) element
    -> Nat (N minAdded (Is min To extendedMin) x)
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin extendedMin) element
extend added minAdded =
    Internal.extend added
        (\addedLen -> MinNat.add addedLen minAdded)


extendN :
    Arr (N added (Is min To extendedMin) x) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin extendedMin) element
extendN arrExtension =
    Internal.extend arrExtension MinNat.addN



-- ## scan length


isLength :
    Nat (N (Nat1Plus triedMinus1) (Is a To (Nat1Plus triedMinus1PlusA)) x)
    -> { min : Nat (N min (Is minToTriedMinus1 To triedMinus1) y) }
    ->
        { equal :
            Arr
                (ValueOnly (Nat1Plus triedMinus1))
                element
            -> result
        , greater :
            Arr (ValueMin (Nat2Plus triedMinus1)) element -> result
        , less :
            Arr (In min triedMinus1PlusA maybeN) element -> result
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
            |> MinNat.is amount
                min
                { equal =
                    \() ->
                        .equal cases
                            (withLength (amount |> InNat.value))
                , greater = withLength >> .greater cases
                , less = withLength >> .less cases
                }


isLengthAtLeast :
    Nat (N lowerBound (Is a To (Nat1Plus lowerBoundMinus1PlusA)) x)
    -> { min : Nat (N min (Is minToTriedMin To lowerBound) y) }
    ->
        { equalOrGreater : Arr (ValueMin lowerBound) element -> result
        , less : Arr (In min lowerBoundMinus1PlusA maybeN) element -> result
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
            |> MinNat.isAtLeast lowerBound
                min
                { less =
                    withLength >> .less cases
                , equalOrGreater =
                    withLength >> .equalOrGreater cases
                }


isLengthAtMost :
    Nat (N upperBound (Is a To upperBoundPlusA) x)
    -> { min : Nat (N min (Is minToAtMostMin To upperBound) y) }
    ->
        { equalOrLess :
            Arr (In min upperBoundPlusA maybeN) element
            -> result
        , greater :
            Arr (ValueMin (Nat1Plus upperBound)) element
            -> result
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
            |> MinNat.isAtMost upperBound
                min
                { equalOrLess =
                    withLength >> .equalOrLess cases
                , greater = withLength >> .greater cases
                }


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
    \arr ->
        let
            { groups, less } =
                toArray arr
                    |> Array.group (val groupSize) direction
        in
        { groups =
            { array =
                groups
                    |> Array.map
                        (\array ->
                            { array = array, length = groupSize }
                                |> tag
                                |> isChecked Internal.Arr
                        )
            , length = length arr |> Nat.div groupSize
            }
                |> tag
                |> isChecked Internal.Arr
        , less =
            { array = less
            , length = length arr |> Nat.remainderBy groupSize
            }
                |> tag
                |> isChecked Internal.Arr
        }


value : Arr (In min max maybeN) element -> Arr (ValueMin min) element
value =
    Internal.mapLength MinNat.value
        >> isChecked Internal.Arr
