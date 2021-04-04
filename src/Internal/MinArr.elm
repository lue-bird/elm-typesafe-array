module Internal.MinArr exposing
    ( extend
    , extendN
    , insertAt
    , isLength
    , isLengthAtLeast
    , push
    , removeAt
    )

import Arr exposing (Arr, length, toArray)
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import MinNat
import NNat
import NNats exposing (..)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import TypeNats exposing (..)


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
    -> Nat (N minAdded (Is min To) extendedMin x)
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin extendedMin) element
extend added minAdded =
    Internal.extend added
        (\addedLen -> MinNat.add addedLen minAdded)


extendN :
    Arr (N added (Is min To) extendedMin x) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueMin extendedMin) element
extendN nArrayExtension =
    Internal.extend nArrayExtension MinNat.addN



-- ## compare


isLength :
    Nat
        (N
            (Nat1Plus triedMinus1)
            (Is (Nat1Plus aMinus1) To)
            (Nat1Plus triedMinus1PlusA)
            x
        )
    -> { min : Nat (N min (Is lessRange To) triedMinus1 y) }
    ->
        { equal :
            Arr
                (N
                    (Nat1Plus triedMinus1)
                    (Is (Nat1Plus aMinus1) To)
                    (Nat1Plus triedMinus1PlusA)
                    x
                )
                element
            -> result
        , greater : Arr (ValueMin (Nat2Plus triedMinus1)) element -> result
        , less : Arr (ValueIn min triedMinus1PlusA) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLength amount min cases =
    \arr ->
        let
            withLength len =
                Internal.Arr (toArray arr) { length = len }
        in
        length arr
            |> MinNat.is (amount |> NNat.toIn)
                min
                { equal =
                    \() -> .equal cases (withLength amount)
                , greater = withLength >> .greater cases
                , less = withLength >> .less cases
                }


isLengthAtLeast :
    Nat (In tried (Nat1Plus triedMinus1PlusA) triedMaybeN)
    -> { min : Nat (N min (Is minToTriedMin To tried) x) }
    ->
        { equalOrGreater : Arr (ValueIn tried max) element -> result
        , less : Arr (ValueIn min triedMinus1PlusA) element -> result
        }
    -> Arr (In min max maybeN) element
    -> result
isLengthAtLeast tried min cases =
    \arr ->
        length arr
            |> MinNat.isAtLeast tried
                min
                { less =
                    \len ->
                        .less cases
                            (Arr (toArray arr) { length = len })
                , equalOrGreater =
                    \len ->
                        .equalOrGreater cases
                            (Arr (toArray arr) { length = len })
                }
