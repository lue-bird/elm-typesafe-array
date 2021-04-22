module Internal.MinArr exposing
    ( extend
    , extendN
    , group
    , insertAt
    , isLength
    , isLengthAtLeast
    , push
    , removeAt
    )

import Arr exposing (Arr, length, toArray)
import Array
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import LinearDirection.Array as Array
import MinNat
import NNat
import NNats exposing (..)
import Nat exposing (In, Is, N, Nat, To, ValueIn, ValueMin)
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



-- ## compare


isLength :
    Nat (In (Nat1Plus triedMinus1) (Nat1Plus atLeastTriedMinus1) maybeN)
    -> { min : Nat (N min (Is minToTriedMinus1 To triedMinus1) x) }
    ->
        { equal :
            Arr
                (In
                    (Nat1Plus triedMinus1)
                    (Nat1Plus atLeastTriedMinus1)
                    maybeN
                )
                element
            -> result
        , more :
            Arr (ValueMin (Nat2Plus triedMinus1)) element -> result
        , less :
            Arr (In min atLeastTriedMinus1 e) element -> result
        }
    -> Arr (In min max e) element
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
            |> MinNat.is (amount |> NNat.toIn)
                min
                { equal =
                    \() -> .equal cases (withLength amount)
                , greater = withLength >> .more cases
                , less = withLength >> .less cases
                }


isLengthAtLeast :
    Nat (In tried (Nat1Plus triedMinus1PlusA) triedMaybeN)
    -> { min : Nat (N min (Is minToTriedMin To tried) x) }
    ->
        { equalOrMore : Arr (ValueMin tried) element -> result
        , less : Arr (In min triedMinus1PlusA maybeN) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtLeast tried min cases =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Internal.Arr
        in
        length arr
            |> MinNat.isAtLeast tried
                min
                { less =
                    \len ->
                        .less cases (withLength len)
                , equalOrGreater =
                    \len ->
                        .equalOrMore cases (withLength len)
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
